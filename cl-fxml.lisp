
(defpackage :cl-fxml
  (:use :common-lisp :agnostic-lizard :named-readtables)
  (:export :*new-line-after-opening* :*indent-size*
	   :syntax :with-xml))

(in-package :cl-fxml)

;;; Distinguisher

(defun xml-p (form)
  (and (consp form) (consp (car form)) (keywordp (caar form))))

;;; Settings

(defvar *indent-size* 4)
(defvar *new-line-after-opening* nil)

;;; Transformation

(defvar *indentation* 0)

(defun process-xml (form walker)
  (if (xml-p form)
    (destructuring-bind ((tag &rest atts) &body tagbody) form
      `(progn
         (format t "~&~v,v@T<~a" *indentation* *indentation* ,tag)
         (incf *indentation* *indent-size*)
         ,@(loop for prev-att = nil then att
                 and att in atts
                 collect `(format t ,(cond ((keywordp att) " ~a=")
                                           ((keywordp prev-att) "\"~a\"")
                                           (t " ~a"))
				  ,att))
         (if *new-line-after-opening*
           (format t ,(if tagbody ">~%" "/>~%"))
           (format t ,(if tagbody "~&~v,v@T>" "~2*/>~%") *indentation* *indentation*))
         ,(let ((result (gensym)))
            `(let ((,result ,(funcall walker tagbody)))
                (when (stringp ,result) (format t (if *new-line-after-opening* "~&~v,v@T~a~%" "~2*~a~%") *indentation* *indentation* ,result))))
         (decf *indentation* *indent-size*)
         ,(when tagbody `(format t "~&~v,v@T</~a>~%" *indentation* *indentation* ,tag))))
    form))

;;; Walking

(defun walk-xml (forms env)
  `(progn
     ,@(loop for form in forms
             collect (agnostic-lizard:walk-form form env 
                       :on-every-form-pre #'(lambda (f e)
                                              (process-xml f #'(lambda (body)
                                                                 (walk-xml body e))))))))

;;; Macro

(defmacro with-xml (&body body &environment env)
  (walk-xml body env))

;;; Read macro

(defun setup (&optional (rt *readtable*))
  (let* ((normal-rt *readtable*)
         (*readtable* rt))
    (agnostic-lizard:install-wrap-every-form-reader
      #'(lambda (form) (cons 'with-xml (list form))))
    (let* ((rt *readtable*)
           (*readtable* normal-rt))
      rt)))

(defvar *extended-readtable* (setup (copy-readtable nil)))

(defreadtable syntax
   (:merge :standard)
   (:syntax-from *extended-readtable* #\( #\()
   (:case :preserve))


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
        `(unwind-protect
              (progn
                (format t "~&~v,0@T<~a"
                        *indentation*
                        ,tag)
                (incf *indentation* *indent-size*)
                ,@(loop for prev-att = nil then att
                     and att in atts
                     collect `(format t "~[~* ~a=~;~*\"~a\"~:;~:[ ~;~]~a~]"
                                      ,(cond ((keywordp att) 0)
                                             ((keywordp prev-att) 1)
                                             (t 2))
                                      ,(eq 0 (position #\[ (subseq (symbol-name tag) (1- (length (symbol-name tag))))))
                                      ,att))
                (format t "~[?>~%~; -->~%~;~2*~a>~%~;~:[~&~v,0@T>~;>~%~]~:;/>~%~]"
                        ,(cond ((eq 0 (position #\? (symbol-name tag))) 0)
                               ((eq tag :!--) 1)
                               ((eq 0 (position #\! (symbol-name tag))) 2)
                               (tagbody 3)
                               (t 4))
                        *new-line-after-opening*
                        *indentation*
                        ,(apply #'concatenate 'string
                                (loop repeat (count #\[ (symbol-name tag))
                                   collect "]")))
                ,(let ((result (gensym)))
                   `(let ((,result ,(funcall walker tagbody)))
                      (when (stringp ,result)
                        (format t "~:[~*~;~&~v,0@T~]~a~%"
                                *new-line-after-opening*
                                *indentation*
                                ,result)))))
           (decf *indentation* *indent-size*)
           ,(when tagbody
              `(format t "~&~v,0@T</~a>~%"
                       *indentation*
                       ,tag))))
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
    (agnostic-lizard:install-wrap-every-form-reader #'(lambda (form)
                                                        (cons 'with-xml (list form))))
    (let* ((rt *readtable*)
           (*readtable* normal-rt))
      rt)))

(defvar *extended-readtable* (setup (copy-readtable nil)))

(defreadtable syntax
    (:merge :standard)
  (:syntax-from *extended-readtable* #\( #\()
  (:case :upcase))

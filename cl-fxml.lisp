
(defpackage :cl-fxml
  (:use :common-lisp :agnostic-lizard :named-readtables)
  (:export :enable-syntax :*new-line-after-opening*))

(in-package :cl-fxml)

;(ql:quickload "agnostic-lizard")

;;; Distinguisher

(defun my-xml-p (form)
  (and (consp form) (consp (car form)) (keywordp (caar form))))

;;; Settings

(defconstant +indent-size+ 4)
(defvar *new-line-after-opening* nil)

;;; Transformation

(defvar *indentation* 0)

(defun process-my-xml (form walker)
  (if (my-xml-p form)
    (destructuring-bind ((tag &rest atts) &body tagbody) form
      `(progn
         (format t (if *new-line-after-opening* "~&~v,v@T<~a" "~2*<~a") *indentation* *indentation* ,tag)
         (incf *indentation* +indent-size+)
         ,@(loop for prev-att = nil then att
                 for att in atts
                 collect `(format t ,(cond ((keywordp att) " ~a=")
                                           ((and (symbolp att) (not (keywordp prev-att))) " ~a")
                                           (t "\"~a\"")) ,att))
         (if *new-line-after-opening*
           (format t ,(if tagbody ">~%" "/>~%"))
           (format t ,(if tagbody "~&~v,v@T>" "/>~%") *indentation* *indentation* ))
         ,(let ((result (gensym)))
            `(let ((,result ,(funcall walker tagbody)))
                (when (stringp ,result) (format t (if *new-line-after-opening* "~&~v,v@T~a~%" "~2*~a~%") *indentation* *indentation* ,result))))
         (decf *indentation* +indent-size+)
         ,(when tagbody `(format t "~&~v,v@T</~a>~%" *indentation* *indentation* ,tag))))
    form))

;;; Walking

(defun walk-my-xml (forms env)
  `(progn
     ,@(loop for form in forms
             collect (agnostic-lizard:walk-form form env 
                       :on-every-form-pre #'(lambda (f e)
                                              (process-my-xml f #'(lambda (body)
                                                                    (walk-my-xml body e))))))))

;;; Macro

(defmacro xml (&body body &environment env)
  (walk-my-xml body env))

;;; Read macro

(defun setup (&optional (rt *readtable*))
  (let* ((normal-rt *readtable*)
         (*readtable* rt))
    (agnostic-lizard:install-wrap-every-form-reader
      #'(lambda (form) #|(format t "~s" (cons 'xml (list form)))|# (cons 'xml (list form))))
    (let* ((rt *readtable*)
           (*readtable* normal-rt))
      rt)))

(defvar *my-readtable* (setup (copy-readtable nil)))

(defmacro in-syntax (rt-expr)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* ,rt-expr)))

(defmacro enable-syntax () (in-syntax *my-readtable*))

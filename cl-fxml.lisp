
(defpackage :cl-fxml
  (:use :common-lisp)
  (:import-from :agnostic-lizard
                :walk-form
                :install-wrap-every-form-reader)
  (:import-from :named-readtables
                :defreadtable)
  (:import-from :alexandria
                :once-only)
  (:export :*new-line-after-opening* :*indent-size*
           :syntax :with-xml))

(in-package :cl-fxml)

;;; Settings

(defvar *indent-size* 4)
(defvar *new-line-after-opening* nil)

;;; Distinguishers

(defun computed-name-p (form)
  (and (consp form)
       (consp (cdr form))
       (eq :|| (car form))))

(defun composed-name-p (form)
  (and (consp form)
       (consp (cdr form))
       (keywordp (car form))))

(defun name-p (form)
  (or (keywordp form)
      (composed-name-p form)))

(defun xml-template-p (form)
  (and (consp form)
       (consp (car form))
       (or (keywordp (caar form))
           (composed-name-p (caar form)))))

;;; Transformation

(defvar *indentation* 0)

(defun qualify-name (first-part &optional second-part)
  (intern 
   (if second-part
       (concatenate 'string
                    (format nil "~a" first-part) '(#\:) (format nil "~a" second-part))
       (format nil "~a" first-part))
   (find-package "KEYWORD")))

(defun process-composed-name (&rest parts)
  (cond ((computed-name-p parts) (apply #'qualify-name (cdr parts)))
        ((composed-name-p parts) (apply #'qualify-name parts))
        (t (car parts))))

(defun attribute-parts-printer (attribute previous-attribute-p element-name)
  `(format t "~[~* ~a=~;~*\"~a\"~:;~*~:[~;~2:*~:[ ~;~]~a~]~]"
           (cond ((name-p ,attribute) 0)
                 (,previous-attribute-p 1)
                 (t 2))
           (eq 0 (position #\[ (subseq (symbol-name ,element-name) (1- (length (symbol-name ,element-name))))))
           ,attribute))

(defun process-xml (form walker)
  (cond ((composed-name-p form) `(process-composed-name ,@form))
        ((xml-template-p form)
         (destructuring-bind ((element-name &rest attributes) &body element-body) form
           (alexandria:once-only
            ((element-name (if (composed-name-p element-name)
                               `(process-composed-name ,@element-name)
                               element-name)))
            `(unwind-protect
                 (progn
                   (format t "~&~v,0@T<~a"
                           *indentation*
                           ,element-name)
                   (incf *indentation* *indent-size*)
                   ,(when attributes
                      `(let (previous-attribute)
                         ,@(loop for attribute in attributes
                              collect (alexandria:once-only
                                      (attribute)
                                      `(prog1
                                           (if (consp ,attribute)
                                               (loop for inner-previous-attribute = nil then inner-attribute
                                                  and inner-attribute in ,attribute
                                                  do ,(attribute-parts-printer 'inner-attribute '(keywordp inner-previous-attribute) element-name))
                                               ,(attribute-parts-printer attribute '(keywordp previous-attribute) element-name))
                                         (setf previous-attribute ,attribute))))))
                   (format t "~[?>~%~; -->~%~;~2*~a>~%~;~:[~&~v,0@T>~;>~%~]~:;/>~%~]"
                           (cond ((eq 0 (position #\? (symbol-name ,element-name))) 0)
                                 ((eq ,element-name :!--) 1)
                                 ((eq 0 (position #\! (symbol-name ,element-name))) 2)
                                 (,(not (null element-body)) 3)
                                 (t 4))
                           *new-line-after-opening*
                           *indentation*
                           (apply #'concatenate 'string
                                  (loop repeat (count #\[ (symbol-name ,element-name))
                                     collect "]")))
                   ,(alexandria:once-only
                     ((result (funcall walker element-body)))
                     `(when (stringp ,result)
                        (format t "~:[~*~;~&~v,0@T~]~a~%"
                                *new-line-after-opening*
                                *indentation*
                                ,result))))
               (decf *indentation* *indent-size*)
               ,(when element-body
                  `(format t "~&~v,0@T</~a>~%"
                           *indentation*
                           ,element-name))))))
        (t form)))

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

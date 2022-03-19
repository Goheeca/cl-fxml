
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

;;; Distinguishers

(defun computed-element-p (form)
  (and (consp form)
       (consp (cdr form))
       (eq :|| (car form))))

(defun composed-element-p (form)
  (and (consp form)
       (consp (cdr form))
       (keywordp (car form))))

(defun attribute-name-p (form)
  (or (keywordp form)
      (composed-element-p form)))

(defun xml-template-p (form)
  (and (consp form)
       (consp (car form))
       (or (keywordp (caar form))
	   (composed-element-p (caar form)))))

;;; Settings

(defvar *indent-size* 4)
(defvar *new-line-after-opening* nil)

;;; Transformation

(defvar *indentation* 0)

(defun namespace-element (first-part &optional second-part)
  (intern 
   (if second-part
       (concatenate 'string
		    (format nil "~a" first-part) '(#\:) (format nil "~a" second-part))
       (format nil "~a" first-part))
   (find-package "KEYWORD")))

(defun process-element (&rest parts)
  (cond ((computed-element-p parts) (apply #'namespace-element (cdr parts)))
	((composed-element-p parts) (apply #'namespace-element parts))
	(t (car parts))))

(defun attribute-printer (att prev-att-p tag)
  `(format t "~[~* ~a=~;~*\"~a\"~:;~*~:[~;~2:*~:[ ~;~]~a~]~]"
	   (cond ((attribute-name-p ,att) 0)
		 (,prev-att-p 1)
		 (t 2))
	   (eq 0 (position #\[ (subseq (symbol-name ,tag) (1- (length (symbol-name ,tag))))))
	   ,att))

(defun process-xml (form walker)
  (cond ((composed-element-p form) `(process-element ,@form))
	((xml-template-p form)
	 (destructuring-bind ((tag &rest atts) &body tagbody) form
	   (alexandria:once-only
	    ((tag (if (composed-element-p tag)
		      `(process-element ,@tag)
		      tag)))
	    `(unwind-protect
		  (progn
		    (format t "~&~v,0@T<~a"
			    *indentation*
			    ,tag)
		    (incf *indentation* *indent-size*)
		    ,(when atts
		       `(let (prev-att)
			  ,@(loop for att in atts
			       collect (alexandria:once-only
					(att)
					`(prog1 
					     (if (consp ,att)
						 (loop for inner-prev-att = nil then inner-att
						    and inner-att in ,att
						    do ,(attribute-printer 'inner-att '(keywordp inner-prev-att) tag))
						 ,(attribute-printer att '(keywordp prev-att) tag))
					   (setf prev-att ,att))))))
		    (format t "~[?>~%~; -->~%~;~2*~a>~%~;~:[~&~v,0@T>~;>~%~]~:;/>~%~]"
			    (cond ((eq 0 (position #\? (symbol-name ,tag))) 0)
				  ((eq ,tag :!--) 1)
				  ((eq 0 (position #\! (symbol-name ,tag))) 2)
				  (,(not (null tagbody)) 3)
				  (t 4))
			    *new-line-after-opening*
			    *indentation*
			    (apply #'concatenate 'string
				   (loop repeat (count #\[ (symbol-name ,tag))
				      collect "]")))
		    ,(alexandria:once-only
		      ((result (funcall walker tagbody)))
		      `(when (stringp ,result)
			 (format t "~:[~*~;~&~v,0@T~]~a~%"
				 *new-line-after-opening*
				 *indentation*
				 ,result))))
	       (decf *indentation* *indent-size*)
	       ,(when tagbody
		  `(format t "~&~v,0@T</~a>~%"
			   *indentation*
			   ,tag))))))
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

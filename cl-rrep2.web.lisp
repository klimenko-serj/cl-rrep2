(in-package #:cl-rrep2.web)
;;------------------------------------------------------------------------------
(defun rcfg-get-name (rcfg)
  (getf rcfg :name))
;;------------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *resources-dir*
    (merge-pathnames "resources/"
                     (asdf:component-pathname (asdf:find-system '#:cl-rrep2))))
  (closure-template:compile-template :common-lisp-backend
                                     (merge-pathnames "cl-rrep2.tmpl"
                                                      *resources-dir*)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *rcfg-dir* (merge-pathnames "rcfg/" (first (directory "")))))
;;------------------------------------------------------------------------------
(defparameter *rcfgs* (make-array 5 :fill-pointer 0 :adjustable t))
;;------------------------------------------------------------------------------
(defun reload-reports (&optional (dir *rcfg-dir*))
  (progn
    (setf *rcfg-dir* dir)
    (setf *rcfgs* (make-array 5 :fill-pointer 0 :adjustable t))
    (mapcar (lambda (x)
	      (vector-push-extend (load-rcfg-from-file x) *rcfgs*))
	    (directory (merge-pathnames "*.rcfg" dir)))))
;;------------------------------------------------------------------------------
(defun build-params-forms-list (params)
  (mapparams 
   (lambda (name val)
     (case (getf (getf val :read-form) :type)
       (:text (cl-rrep2.view:param-text 
	       (list :key name 
		     :caption (getf (getf val :read-form) :caption))))
       (:checklist (cl-rrep2.view:param-list 
		    (list :key name
			  :caption (getf (getf val :read-form) :caption)
			  :list (getf (getf val :read-form) :list))))))
   params))
;;------------------------------------------------------------------------------
(defun get-params-from-post-params (params post-params)
 (mapparamsn
  (lambda (name val)
    (case (getf (getf val :read-form) :type)
      (:text (list name (cdr (assoc (symbol-name name) post-params :test #'string=))))
      (:checklist 
       (list name
	     (remove-if #'NULL 
			(mapcar 
			 (lambda (x)
			   (if (member (cons (symbol-name name) 
					     (format nil "~a"(getf x :key))) 
				       post-params :test #'equal)
			       (getf x :value)
			       Nil))
			 (getf (getf val :read-form) :list)))))))
  params))
;;------------------------------------------------------------------------------
(defun generate-param-form (rcfg)
    (cl-rrep2.view:params-form 
     (list :params (build-params-forms-list (get-updated-params rcfg)))))
;;------------------------------------------------------------------------------
(restas:define-route rrep-param ("/:id")
  (generate-param-form (elt *rcfgs* (parse-integer id))))
;;------------------------------------------------------------------------------
(restas:define-route rrep-build ("/:id" :method :post)
  (generate-html-report 
   (elt *rcfgs* (parse-integer id))
   (get-params-from-post-params (get-updated-params 
				 (elt *rcfgs* (parse-integer id))) 
				(hunchentoot:post-parameters*))))
;;------------------------------------------------------------------------------
(restas:define-route rrep-main ("")
  (cl-rrep2.view:main-frame 
   (list :title "rrep" 
	 :reports 
	 (loop for i from 0 to (- (length *rcfgs*) 1)
	    collect (list :name (rcfg-get-name (elt *rcfgs* i))
			  :href (restas:genurl 'rrep-param :id i))))))
;;------------------------------------------------------------------------------
(defun rrep2.web-start (&optional (port 8080))
  (progn
    (reload-reports (merge-pathnames "rcfg/" (first (directory ""))))
    (restas:start '#:cl-rrep2.web :port port)))
;;------------------------------------------------------------------------------

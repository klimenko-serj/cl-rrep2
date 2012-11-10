;;;; package.lisp

;;------------------------------------------------------------------------------
(defpackage #:cl-rrep2
  (:use #:cl)
  (:export :generate-html-report
	   :get-updated-params
	   :load-rcfg-from-file
	   :mapparams
	   :mapparamsn
   ))
;;------------------------------------------------------------------------------
(restas:define-module #:cl-rrep2.web
  (:use #:cl 
	#:cl-rrep2 
	#:closure-template)
  (:export :*resources-directory*
           :*rcfg-directory*
           :reload-reports
           :rrep2.web-start
   ))
;;------------------------------------------------------------------------------


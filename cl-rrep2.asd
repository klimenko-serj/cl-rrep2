;;;; cl-rrep2.asd

(asdf:defsystem #:cl-rrep2
  :serial t
  :description "Describe cl-rrep2 here"
  :author "Klimenko Serj <klimenko.serj@gmail.com>"
  :license "MIT"
  :depends-on (#:restas #:closure-template #:cl-fbclient #:restas-directory-publisher)
  :components ((:file "package")
               (:file "cl-rrep2")
	       (:file "cl-rrep2.web")))


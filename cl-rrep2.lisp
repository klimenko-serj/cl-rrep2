;;;; cl-rrep2.lisp

(in-package #:cl-rrep2)

;;; "cl-rrep2" goes here. Hacks and glory await!
;;------------------------------------------------------------------------------
(defun recmap (fnp fn lst)
  (mapcar
   #'(lambda (x)
	      (cond  
		((atom x) x)
		((funcall fnp x) (funcall fn x))
		(T (recmap fnp fn x))))
	  lst))
;;------------------------------------------------------------------------------
(defun recmapn (fnp fn lst)
  (mapcan
   #'(lambda (x)
	      (cond  
		((atom x) (list x))
		((funcall fnp x) (funcall fn x))
		(T (list (recmapn fnp fn x)))))
	  lst))
;;------------------------------------------------------------------------------
(defmacro fnp-car-eq (eqsymb)
  `#'(lambda (x) 
       (eq (car x) ,eqsymb)))
;;------------------------------------------------------------------------------
(defun strlist2str (l)
  (cond ((NULL l) "")
	((atom l) (format nil "~a" l))
	(T (format nil "~A~A" 
		   (car l)
		   (strlist2str (cdr l))))))
;;------------------------------------------------------------------------------
(defun make-string-by-list-or-string (q)
  (if (stringp q) q
      (strlist2str q)))
;;------------------------------------------------------------------------------
;; RCFG
;;------------------------------------------------------------------------------
(defun load-rcfg-from-file (file)
  (with-open-file (fs file :external-format :UTF-8)
    (read fs)))
;;------------------------------------------------------------------------------
(defun rcfg-get-report (rcfg)
  (getf rcfg :report))
;;------------------------------------------------------------------------------
(defun rcfg-get-macros (rcfg)
  (getf rcfg :macros))
;;------------------------------------------------------------------------------
(defun rcfg-get-db (rcfg)
  (getf rcfg :database))
;;------------------------------------------------------------------------------
(defun rcfg-get-params (rcfg)
  (getf rcfg :params))
;;------------------------------------------------------------------------------
;; TABLES (layouts)
;;------------------------------------------------------------------------------
(defclass table-item ()
  ((left :accessor left
	 :initform 0)
   (top :accessor top
	:initform 0)
   (width :accessor width
	  :initform 0)
   (height :accessor height
	   :initform 0)))
;;------------------------------------------------------------------------------
(defgeneric update-size (itm))
(defgeneric write-to-cells-table (itm cells))
;;------------------------------------------------------------------------------
(defclass table-layout (table-item)
  ((orientation :accessor orientation
		:initform :vertical
		:initarg :orientation)
   (subitems :accessor subitems
	     :initform Nil)))
;;------------------------------------------------------------------------------
(defmethod update-size ((itm table-layout))
  (update-layout-stretch itm))
;;------------------------------------------------------------------------------
(defmethod write-to-cells-table ((itm table-layout) cells)
  (loop for sitm in (subitems itm) do
       (progn 
	 (incf (left sitm) (left itm))
	 (incf (top sitm) (top itm))
	 (write-to-cells-table sitm cells))))
;;------------------------------------------------------------------------------
(defclass table-cell (table-item)
  ((value :accessor value
	 :initform ""
	 :initarg :value)))
;;------------------------------------------------------------------------------
(defmethod update-size ((itm table-cell)))
(defmethod write-to-cells-table ((itm table-cell) cells)
  (setf (aref cells (left itm) (top itm)) itm))
;;------------------------------------------------------------------------------
(defmacro if-orient-vh (lay &body body)
  `(if (eq (orientation ,lay) :vertical) 
       ,@body))
;;------------------------------------------------------------------------------
(defmethod initialize-instance :after 
    ((cell table-cell) &key parent-orientation params)
  (progn 
    (setf (value cell) (getf params :value))
    (setf (width cell) 
	  (if (eq parent-orientation :vertical) 
	      (or (getf params :stretch)
		  1)
	      1))
    (setf (height cell)
	  (if (eq parent-orientation :horizontal) 
	      (or (getf params :stretch)
		  1)
	      1))))
;;------------------------------------------------------------------------------
(defun set-width-height-one (lay)
  (if-orient-vh lay
    (setf (width lay) 1)
    (setf (height lay) 1)))
;;------------------------------------------------------------------------------
(defun update-layout-stretch (lay)
  (loop for itm in (subitems lay) do
       (if-orient-vh lay
	 (when (> (width lay) (width itm))
	   (progn 
	     (setf (width itm) (width lay))
	     (update-size itm)))
	 (when (> (height lay) (height itm))
	   (progn 
	     (setf (height itm) (height lay))
	     (update-size itm))))))
;;------------------------------------------------------------------------------
(defun process-item (itm parent-orientation)
  (cond ((eq (car itm) :cell)
	 (make-instance 'table-cell 
			:parent-orientation parent-orientation 
			:params (cdr itm)))
	((eq (car itm) :layout)
	 (make-instance 'table-layout 
			:orientation (car (cdr itm)) 
			:items (cddr itm)))))
;;------------------------------------------------------------------------------
(defun add-item (lay itm)
  (progn
    (if-orient-vh lay
      (progn
	(setf (top itm) (height lay))
	(when (> (width itm) (width lay)) 
	  (setf (width lay) (width itm)))
	(incf (height lay) (height itm)))
      (progn
	(setf (left itm) (width lay))
	(when (> (height itm) (height lay)) 
	  (setf (height lay) (height itm)))
	(incf (width lay) (width itm))))
    (setf (subitems lay) (append (subitems lay) (list itm)))))
;;------------------------------------------------------------------------------
(defmethod initialize-instance :after ((lay table-layout) &key items)
  (progn 
    (set-width-height-one lay)
    (loop for itm in items do
	 (add-item lay (process-item itm (orientation lay))))
    (update-layout-stretch lay)))
;;------------------------------------------------------------------------------
(defun make-table-main-layout (params)
  (make-instance 'table-layout 
		 :orientation (car params) 
		 :items (cdr params)))
;;------------------------------------------------------------------------------
(defun make-cells-table (lay)
  (let ((ct (make-array (list (width lay) (height lay)))))
    (write-to-cells-table lay ct)
  ct))
;;------------------------------------------------------------------------------
(defun cells-table-to-html (cells)
  (format nil "<table BORDER CELLPADDING=4 CELLSPACING=0>~A</table>"
	  (with-output-to-string (stream)
	  (dotimes (y (array-dimension cells 1))
	    (format stream "<tr valign=middle halign=middle>~A</tr>"
		    (with-output-to-string (stream)
		    (dotimes (x (array-dimension cells 0))
		      (when (not (numberp (aref cells x y)))
			(format stream "<td colspan=~A rowspan=~A >~A</td>"
				(width (aref cells x y))
				(height (aref cells x y))  
				(make-string-by-list-or-string 
				 (value (aref cells x y))))))))))))
;;------------------------------------------------------------------------------
(defun make-table (rcfg-table)
  (cells-table-to-html
   (make-cells-table
    (make-table-main-layout rcfg-table))))
;;------------------------------------------------------------------------------
(defun generate-html-report-rec (report)
  (cond ((NULL report) "")
	((stringp report) report)
	((atom (car report))
	 (format nil "~A~A" (car report)
		 (generate-html-report-rec (cdr report))))
	((listp report)
	 (cond ((eq (caar report) :table)
		(format nil "~A~A "
			(make-table (cdar report)) 
			(generate-html-report-rec (cdr report))))
	       (T Nil)))
	(T Nil)))
;;------------------------------------------------------------------------------
;; MACROS
;;------------------------------------------------------------------------------
(defun update-macros (report macros)
  (recmap (fnp-car-eq :macro) 
	  #'(lambda (x)
	      (getf macros (cadr x)))
	  report))
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; DBREADER
;;------------------------------------------------------------------------------
(defun rrep-update-by-query-result (qname qvals body)
  (recmap (fnp-car-eq qname) 
	  #'(lambda (x)
	      (getf qvals (cadr x)))
	  body))
;;------------------------------------------------------------------------------
(defun make-p-list (Lvars Lvals)
  (cond
    ((or (NULL LVARS) (NULL LVALS)) Nil)
    (T (append (list (car lvars) (car lvals))
	       (make-p-list (cdr lvars) (cdr lvals))))))
;;------------------------------------------------------------------------------
(defun rrep-with-query (body db)
  (mapcan 
   (lambda (qr)
     (rrep-update-by-query-result 
      (cadr body)
      (make-p-list (caddr body) qr)
      (cdddr body)))
   (cl-fbclient:fb-query 
    (make-string-by-list-or-string (car body)) 
    :db db)))
;;------------------------------------------------------------------------------
(defun update-queryes-rec (report db)
   (cond ((NULL report) Nil)
	((atom (car report))
	 (cons (car report) (update-queryes-rec (cdr report) db)))
	((eq (caar report) :query)
	 (append (update-queryes-rec (rrep-with-query (cdar report) db) db)  
		 (update-queryes-rec (cdr report) db)))
	(T
	 (cons (update-queryes-rec (car report) db)
	       (update-queryes-rec (cdr report) db)))))
;;------------------------------------------------------------------------------
(defun update-queryes (report db)
  (cl-fbclient:fb-with-database 
   (rrep-database :host (getf db :host)
		    :path (getf db :path)
		    :user-name (getf db :user-name)
		    :password (getf db :password))
   (update-queryes-rec report rrep-database)))
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; PARAMS
;;------------------------------------------------------------------------------
(defun update-by-param-list-item (param-name list-item report)
  (recmap (fnp-car-eq param-name) 
	  #'(lambda (x)
	      (if (NULL (cdr x))
		  list-item
		  (getf list-item (cadr x))))
	  report))
;;------------------------------------------------------------------------------
(defun update-by-param (report param-name params)
  (update-params 
   (let ((p (getf params param-name)))
     (cond ((NULL p) report)
	   ((NULL report) (list p))
	   (T (mapcan 
	       (lambda (x)
		 (update-by-param-list-item param-name x report))
	       p))))
   params))
;;------------------------------------------------------------------------------
(defun update-params (report params)
  (recmapn (fnp-car-eq :param) 
	  #'(lambda (x)
	      (update-by-param (cddr x) (cadr x) params))
	  report))
;;------------------------------------------------------------------------------
(defun mapparams (fn params)
  (cond ((null params) nil)
	(T (cons (funcall fn (car params) (cadr params))
		 (mapparams fn (cddr params))))))
;;------------------------------------------------------------------------------
(defun mapparamsn (fn params)
  (cond ((null params) nil)
	(T (nconc (funcall fn (car params) (cadr params))
		 (mapparamsn fn (cddr params))))))
;;------------------------------------------------------------------------------
(defun get-updated-params (rcfg)
  (update-queryes
    (update-macros (rcfg-get-params rcfg)
		   (rcfg-get-macros rcfg))
    (rcfg-get-db rcfg)))
;;------------------------------------------------------------------------------
(defun append-params-by-defaults (params rcfg)
  (mapparamsn 
   (lambda (name val)
     (if (getf params name)
	 (list name (getf params name))
	 (list name (getf val :default))))
   (get-updated-params rcfg)))
;;------------------------------------------------------------------------------
;; SUMMATORs
;;------------------------------------------------------------------------------
(defun update-summators (report)
  (let ((summators (make-hash-table)))
    (recmapn 
     (fnp-car-eq :summator)
     (lambda (x)
       (cond 
	 ((NULL (cddr x)) (list (gethash (cadr x) summators)))
	 ((eq (caddr x) :clear) (setf (gethash (cadr x) summators) 0))
	 ((eq (caddr x) :add) (incf (gethash (cadr x) summators) (cadddr x)))
	 ((eq (caddr x) :set) (setf (gethash (cadr x) summators) (cadddr x)))))
     report)))
;;------------------------------------------------------------------------------
;; EVAL	  
;;------------------------------------------------------------------------------
(defun update-evals (report)
  (recmap (fnp-car-eq :eval) 
	  #'(lambda (x)
	      (eval (cdr x)))
	  report))
;;------------------------------------------------------------------------------
;; MAIN: GENERATE-HTML-REPORT	  
;;------------------------------------------------------------------------------
(defun generate-html-report (rcfg params)
 (generate-html-report-rec
  (update-evals
   (update-summators
    (update-queryes 
     (update-params 
      (update-macros (rcfg-get-report rcfg) (rcfg-get-macros rcfg))
      (append-params-by-defaults params rcfg))
    (rcfg-get-db rcfg))))))
;;------------------------------------------------------------------------------

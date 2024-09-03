(in-package #:ryo/statistics)

(declaim (inline possibility))
(defun possibility (p)
  "Test if U(p) is possible. "
  (cond ((< p 0) nil)
	((> p 1) t)
	(t (< (random 1.0) p))))

(defun random-samples (list n &optional (end (max (length list) n)) (samples ()))
  "Randomly choose `n' samples from `list'.

Example:

    (random-samples '(1 2 3 4 5) 3)
"
  (cond ((= n end) (nconc samples list))
	((endp list) samples)
	(t
	 (if (possibility (float (/ n end)))
	     (random-samples (rest list) (1- n) (1- end) (cons (first list) samples))
	     (random-samples (rest list) n (1- end) samples)))))

(defun ascii-table-form (table-list &key (stream t) (element-formatter "~A") (align :right) (width 0))
  "Format 2D list as ASCII table.

The `table-list' should be like:

    ((a11 a12 a13 ...)
     (a21 a22 a23 ...)
     ...)

Each grid element will be formatted with `element-formatter',
Example:

    (ascii-table-form table-list :element-formatter \"~,3f\")
    (ascii-table-form table-list :element-formatter '(\"~,3f\" \"~d\"))

Each column will be aligned by `align',
Example:

    (ascii-table-form table-list :align :center)
    (ascii-table-form table-list :align '(:center :right :left))

Each column by default is in `width',
Example:

    (ascii-table-form table-list :width 10)
    (ascii-table-form table-list :width '(10 15 3))

However, the width will be overwritten to make room for grid element.

The `stream' is where the table will be formatted.
"
  (let* ((cols       (length (first table-list)))
	 (col-idx    (loop for i below cols collect i))
	 ;; compute each align formatter
	 (col-align* (flet ((align-format (align)
			      (case align
				(:center "~~~d:@<~~A~~>")
				(:left   "~~~dA")
				(:right  "~~~d@A"))))
		       (if (atom align)
			   (make-list cols :initial-element (align-format align))
			   (mapcar #'align-format align))))
	 ;; compute each element-formatter
	 (element-formatter* (if (atom element-formatter)
				 (make-list cols :initial-element element-formatter)
				 element-formatter))
	 (col-width* (if (atom width)
			 (make-array cols :element-type 'unsigned-byte
					  :initial-element width)
			 (make-array cols :element-type 'unsigned-byte
					  :initial-contents width))))
	 (loop for row in table-list
	  ;; format table grid and update column max width
	  collect (mapcar
		   (lambda (elem idx element-formatter)
		     (let* ((formatted (format nil element-formatter elem))
			    (col-width (length formatted)))
		       (when (> col-width (aref col-width* idx))
			 (setf (aref col-width* idx) col-width))
		       formatted))
		   row col-idx element-formatter*)
	    into formatted-row
	  finally (let ((formatter (format nil "~~{~~{|~{ ~A~^ |~} |~~}~~^~~%~~}"
					   (map 'list
						(lambda (col-width col-align)
						  (format nil col-align col-width))
						col-width* col-align*))))
		    (format stream formatter formatted-row)))))

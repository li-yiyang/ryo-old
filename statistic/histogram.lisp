(in-package :ryo.statistics)

;; Protocol

(defgeneric hist-bins (hist &rest axies)
  (:documentation
   "Get bin number of given `hist'.

If not given `axies' information:
+ For 1d histogram, return bin number;
+ For 2d histogram, return (x-bins y-bins);
+ For n-dimensional histogram, return a list of dimensions.

If given `axies' information:
Return given axies bin number.
And if axies is not found, ignore axies information. "))

(defgeneric hist-min (hist &rest axies)
  (:documentation
   "Return the min range of given `hist'.

If not given `axies' information:
+ For 1d histogram, return bin number;
+ For 2d histgoram, return (x-min y-min);
+ For n-dimensional histogram, return a list of mins.

If given `axies' information:
Return given axies min value.
And if axies is not found, ignore axies information. "))

(defgeneric hist-max (hist &rest axies)
  (:documentation
   "Return the max range of given `hist'.

If not given `axies' information:
+ For 1d histogram, return bin number;
+ For 2d histgoram, return (x-max y-max);
+ For n-dimensional histogram, return a list of maxs.

If given `axies' information:
Return given axies max value.
And if axies is not found, ignore axies information. "))

(defgeneric add-to-hist (hist val)
  (:documentation "Add `val' to `histogram'. "))

(defgeneric %add-to-hist (hist val)
  (:documentation "Add `val' to `hist' hist slot. "))

(defgeneric hist-rebin! (hist)
  (:documentation
   "Rebin `hist'.

This should (and will) be called after changing histogram size. "))

(defgeneric hist-clear-buffer! (hist)
  (:documentation
   "Clear `hist' buffer. "))

(defgeneric hist-to-csv (hist path &key if-exists)
  (:documentation "Write `hist' to CSV file to `path'. "))

(defgeneric hist-to-ascii (hist &key stream &allow-other-keys)
  (:documentation "Output `hist' to `stream'. "))

(defclass histogram ()
  ((min  :initarg :min :initform -1.0)
   (max  :initarg :max :initform  1.0)
   (tick :initform 0.02)
   (bins :initarg :bins :initform  100)
   (buffer :initform ())
   hist)
  (:documentation
   "A bucketed histogram.

Use `make-histogram' function to create 1D histogram from 1D list data.
It is also recommanded to use `make-histogram' even if you don't have
precollected data like this:

    (make-histogram () :min ... :max ..)

Then use `add-to-hist' function to add more data to histogram instance.

    (add-to-hist hist val)

Use `hist-max', `hist-min' and `hist-bins' to access hist attributes.

Use `hist-to-csv' and `hist-to-ascii' for rendering the data.

For `histogram', the `hist-to-ascii' will return histogram plot in
vertical chart.
"))

(defmethod initialize-instance :after ((hist histogram) &key)
  (hist-rebin! hist))

(defun make-histogram (data &rest args &key (min -1.0) (max 1.0) (bins 100))
  "Make a 1D histogram.

The data should be like:

    (a1 a2 a3 ...)

The `min' and `max' sets limits to histogram sampling range [min, max),
Example:

    (make-histogram '(1 2 3 4) :min 2 :max 4)
    ;; then 1 and 4 will not be sampled

The `bins' specify how many bins should the histogram use.

By default, `min' is -1.0 and `max' is 1.0, `bins' is 100.
"
  (declare (ignorable min max bins))
  (let ((histo (apply #'make-instance 'histogram args)))
    (loop for dat in data do
      (add-to-hist histo dat))
    histo))

(defmethod hist-bins ((hist histogram) &rest axies)
  (declare (ignore axies))
  (slot-value hist 'bins))

(defmethod hist-min ((hist histogram) &rest axies)
  (declare (ignore axies))
  (slot-value hist 'min))

(defmethod hist-max ((hist histogram) &rest axies)
  (declare (ignore axies))
  (slot-value hist 'max))

(defmethod hist-rebin! ((histo histogram))
  (with-slots (min max tick bins hist buffer) histo
    (let* ((%min (min max min))
	   (%max (max max min)))
      ;; clear hist and recalculate hist range
      (setf min %min
	    max %max
	    tick (float (/ (- %max %min) bins))
	    hist (make-array bins :initial-element 0 :element-type 'unsigned-byte))
      ;; resample buffer to hist
      (loop for val in buffer do
	(%add-to-hist hist val)))))

(defmethod %add-to-hist ((histo histogram) val)
  (with-slots (hist min max tick) histo
    (when (and (<= min val) (< val max))
      (incf (aref hist (floor (/ (- val min) tick)))))))

(defmethod add-to-hist :before ((hist histogram) val)
  (push val (slot-value hist 'buffer)))

(defmethod add-to-hist ((hist histogram) val)
  (%add-to-hist hist val))

(defmethod hist-clear-buffer! ((hist histogram))
  (setf (slot-value hist 'buffer) ())
  (hist-rebin! hist))

(defmethod hist-to-csv ((histo histogram) path &key (if-exists :supersede))
  (with-slots (hist bins) histo
    (with-open-file (csv path :direction :output :if-exists if-exists)
      (loop for i below bins
	    do (format csv "~d~%" (aref hist i))))))

(defmethod hist-to-ascii ((histo histogram) &key (stream t) &allow-other-keys)
  (with-slots (hist bins min max) histo
    (let ((cmax 0)
	  (width 30))
      (loop for i below bins
	    for count = (aref hist i)
	    if (> count cmax)
	      do (setf cmax count)
	    collect count into counts
	    finally (format stream "~&* min: ~A~%~{| ~{~A ~D~}~^~%~}~&* max: ~A~%"
			    min
			    (mapcar (lambda (count)
				      (list (make-string (floor (* (/ count cmax) width))
							 :initial-element #\#)
					    count))
				    counts)
			    max)))))

(defclass 2d-histogram (histogram)
  ((min   :initarg :x-min :initform -1.0)
   (y-min :initarg :y-min :initform -1.0)
   (max   :initarg :x-max :initform  1.0)
   (y-max :initarg :y-max :initform  1.0)
   (y-tick                :initform 0.02)
   (bins   :initarg :x-bins :initform 100)
   (y-bins :initarg :y-bins :initform 100)
   (buffer :initform ())
   hist)
  (:documentation
   "A 2D bucketed histogram.

Use `make-2d-histogram' function to create 1D histogram from 1D list data.
It is also recommanded to use `make-histogram' even if you don't have
precollected data like this:

    (make-2d-histogram () :x-min ... :y-min ... :x-max ... :y-max ...)

Then use `add-to-hist' function to add more data to histogram instance.

    (add-to-hist hist val) ; val shall be like '(x y)

Use `hist-max', `hist-min' and `hist-bins' to access hist attributes.

Use `hist-to-csv' and `hist-to-ascii' for rendering the data.

For `2d-histogram', the `hist-to-ascii' will return 2d-histogram plot in
ASCII art image.
"))

(defun make-2d-histogram (data &rest args &key (x-min -1.0) (x-max 1.0)
					    (y-min -1.0) (y-max 1.0)
					    (x-bins 100) (y-bins 100))
  "Make a 1D histogram.

The data should be like:

    ((x1 y1) (x2 y2) (x3 y3) ...)

The `x-min', `y-min', `x-max' and `y-max' sets limits to histogram sampling
range [min, max),
Example:

    (make-2d-histogram '((1 1) (2 2)) :x-min 2 :x-max 4 :y-min ... :y-max ...)
    ;; so (1 1) will not be sampled

The `bins' specify how many bins should the histogram use.

By default, `min' is -1.0 and `max' is 1.0, `bins' is 100.
"
  (declare (ignorable x-min x-max y-min y-max x-bins y-bins))
  (let ((histo (apply #'make-instance '2d-histogram args)))
    (loop for dat in data do
      (add-to-hist histo dat))
    histo))

(defmacro with-axies-idx (axies &rest values)
  "Map `values' according to `axies'. "
  (let ((count (length values)))
    `(mapcar (lambda (i)
	       (ecase (mod i ,count)
		 ,@(loop for i from 0
			 for val in values
			 collect (list i val))))
	     ,axies)))

(defmethod hist-bins ((histo 2d-histogram) &rest axies)
  (with-slots (bins y-bins) histo
    (if (endp axies)
	(list bins y-bins)
	(with-axies-idx axies bins y-bins))))

(defmethod hist-max ((histo 2d-histogram) &rest axies)
  (with-slots (max y-max) histo
    (if (endp axies)
	(list max y-max)
	(with-axies-idx axies max y-max))))

(defmethod hist-min ((histo 2d-histogram) &rest axies)
  (with-slots (min y-min) histo
    (if (endp axies)
	(list min y-min)
	(with-axies-idx axies min y-min))))

(defmethod hist-rebin! ((histo 2d-histogram))
  (with-slots (min y-min max y-max tick y-tick
	       bins y-bins hist buffer)
      histo
    (let* ((%min (min max min))
	   (%max (max max min))
	   (%y-min (min y-max y-min))
	   (%y-max (max y-max y-min)))
      ;; clear hist and recalculate hist range
      (setf min    %min
	    max    %max
	    y-min  %y-min
	    y-max  %y-max
	    tick   (float (/ (- %max %min) bins))
	    y-tick (float (/ (- %y-max %y-min) bins))
	    hist   (make-array (list bins y-bins)
			       :initial-element 0
			       :element-type 'unsigned-byte))
      ;; resample buffer to hist
      (loop for val in buffer do
	(%add-to-hist hist val)))))

(defmethod %add-to-hist ((histo 2d-histogram) val)
  (with-slots (min y-min max y-max tick y-tick hist) histo
    (let ((x (first val))
	  (y (second val)))
      (when (and (<= min x) (< x max)
		 (<= y-min y) (< y y-max))
	(incf (aref hist
		    (floor (/ (- x min) tick))
		    (floor (/ (- y y-min) y-tick))))))))

(defmethod hist-to-csv ((histo 2d-histogram) path &key (if-exists :supersede))
  (with-slots (hist bins y-bins) histo
    (with-open-file (csv path :direction :output :if-exists if-exists)
      (loop for i below y-bins
	    do (loop for j below (1- bins)
		     do (format csv "~d, " (aref hist i j))
		     finally (format csv "~d~%" (aref hist i j)))))))

(defun ascii-grayscale (uniform)
  "Return ASCII character according to `uniform' scale.
The `uniform' should be within [0, 1) range. "
  (let* ((grayscale " .'`^\",:;Il!i~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$")
	 (level     (1- (length grayscale))))
    (aref grayscale (truncate (* level uniform)))))

(defmethod hist-to-ascii ((histo 2d-histogram)
			  &key (stream t) (color-function #'ascii-grayscale)
			  &allow-other-keys)
  (with-slots (x-min y-min x-max y-max bins y-bins hist) histo
    (let ((cmax 0))
      (loop for i below y-bins
	    collect (loop for j below bins
			  for count = (aref hist i j)
			  if (> count cmax)
			    do (setf cmax count)
			  collect count)
	      into grids
	    finally (format stream
			    "~{~{~C~}~%~}"
			    (mapcar (lambda (row)
				      (mapcar (lambda (count)
						(funcall color-function (float (/ count cmax))))
					      row))
				    grids))))))

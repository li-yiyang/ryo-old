(in-package :ryo/statistics)

;; This file defines 2D histogram class `2d-histogram'.
;; see `histogram.lisp' for Develop Notes.

(defclass 2d-histogram (histogram)
  ((min   :initarg :x-min :initform -1.0)
   (y-min :initarg :y-min :initform -1.0)
   (max   :initarg :x-max :initform  1.0)
   (y-max :initarg :y-max :initform  1.0)
   (y-tick                :initform 0.02)
   (bins   :initarg :x-bins :initform 100)
   (y-bins :initarg :y-bins :initform 100)
   (buffer-hist  :initform ())
   (buffer-cache :initform ())
   hist)
  (:documentation
   "A 2D bucketed histogram.

Use `make-2d-histogram' function to create 1D histogram from 1D list data.
It is also recommanded to use `make-histogram' even if you don't have
precollected data like this:

    (make-2d-histogram () :x-min ... :y-min ... :x-max ... :y-max ...)

Then use `add-to-hist' function to add more data to histogram instance.

    (add-to-hist hist val) ; val shall be like '(x y)

To iter over histogram instance data, use `hist-iter-over':

    (hist-iter-over hist (lambda (count x-left y-bottom x-right y-top) ...))

where `count' is the histogram sampling count number;
`left' and `right' is the sampling bin range [left, right).

Use `hist-max', `hist-min' and `hist-bins' to access hist attributes.

Use `hist-to-csv' and `hist-to-ascii' for rendering the data.

For `2d-histogram', the `hist-to-ascii' will return 2d-histogram plot in
ASCII art image.


For developer:
the data within hist is stored as a 2D array (slot `hist'):


    #2A((<x-min y-min> -> <x-max y-min>)
        ( ...                          )
        (<x-min y-max> -> <x-max y-max>))

the index should be refered as (aref hist y x).
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

(defmethod hist-bins ((histo 2d-histogram) &rest axies)
  (with-slots (bins y-bins) histo
    (if (endp axies)
	(list bins y-bins)
	(with-axies-idx axies bins y-bins))))

(defmethod (setf hist-bins) (bins (histo 2d-histogram) &rest axies)
  (let ((bins (if (listp bins) (mapcar #'truncate bins)
		  (make-list 2 :initial-element (truncate bins)))))
    (hist-axies-setf histo axies bins bins y-bins)))

(defmethod hist-min ((histo 2d-histogram) &rest axies)
  (with-slots (min y-min) histo
    (if (endp axies)
	(list min y-min)
	(with-axies-idx axies min y-min))))

(defmethod (setf hist-min) (min (histo 2d-histogram) &rest axies)
  (hist-axies-setf histo axies min min y-min))

(defmethod hist-max ((histo 2d-histogram) &rest axies)
  (with-slots (max y-max) histo
    (if (endp axies)
	(list max y-max)
	(with-axies-idx axies max y-max))))

(defmethod (setf hist-max) (max (histo 2d-histogram) &rest axies)
  (hist-axies-setf histo axies max max y-max))

(defmethod hist-rebin! ((histo 2d-histogram))
  (with-slots (min y-min max y-max tick y-tick
	       bins y-bins hist buffer-hist buffer-cache)
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
      ;; return histo self
      histo)))

(defmethod %add-to-hist ((histo 2d-histogram) val)
  (with-slots (min y-min max y-max tick y-tick hist) histo
    (let ((x (first val))
	  (y (second val)))
      (when (and (<= min x) (< x max)
		 (<= y-min y) (< y y-max))
	(let ((x (floor (/ (- x min) tick)))
	      (y (floor (/ (- y y-min) y-tick))))
	  (incf (aref hist y x))
	  (values x y))))))

(defmethod hist-iter-over ((histo 2d-histogram) fn &key (use-index t))
  (with-slots (hist bins y-bins min y-min tick y-tick) histo
    (if use-index
	(loop for y below y-bins do
	  (loop for x below bins do
	    (funcall fn (at hist x y) x y)))
	(loop for y below y-bins do
	  (loop for x below bins do
	    (funcall fn (at hist x y)
		     (+ min (* tick x)) (+ y-min (* y-tick y))
		     (+ min (* tick (1+ x))) (+ y-min (* y-tick (1+ y)))))))))

(defmethod hist-to-csv ((histo 2d-histogram) path &key (if-exists :supersede))
  (with-slots (hist bins y-bins) histo
    (with-open-file (csv path :direction :output :if-exists if-exists)
      (loop for y from (1- y-bins) downto 0
	    do (loop for x below (1- bins)
		     do (format csv "~d, " (aref hist y x))
		     finally (format csv "~d~%" (aref hist y x)))))))

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
      (loop for y from (1- y-bins) downto 0
	    collect (loop for x below bins
			  for count = (aref hist y x)
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

(defmethod hist-mean ((histo 2d-histogram)
		      &key (density-fn #'identity)
			(offset :center))
  (with-slots (hist min tick y-min y-tick) histo
    (let ((x-numerator 0.0)
	  (y-numerator 0.0)
	  (denominator 0.0)
	  (x-offset    0.5)
	  (y-offset    0.5))
      ;; update offset for constant offset
      (case offset
	(:left   (setf x-offset 0.0
		       y-offset 0.0))
	(:right  (setf x-offset 1.0
		       y-offset 1.0))
	(:center (setf x-offset 0.5
		       y-offset 0.5))
	(otherwise
	 (cond ((listp offset)
		(destructuring-bind (x-o y-o) offset
		  (setf x-offset (if (numberp x-o) x-o 0.5)
			y-offset (if (numberp y-o) y-o 0.5))))
	       ((numberp offset)
		(setf x-offset offset
		      y-offset offset)))))
	(flet ((inc (count x y)
	       (let ((density (funcall density-fn count)))
		 (incf x-numerator (* density (+ x x-offset)))
		 (incf y-numerator (* density (+ y y-offset)))
		 (incf denominator density)))
	     (inc-fn (count x y)
	       (let ((density (funcall density-fn count)))
		 (destructuring-bind (x-offset y-offset)
		     (funcall offset x y)
		   (incf x-numerator (* density (+ x x-offset)))
		   (incf y-numerator (* density (+ y y-offset)))
		   (incf denominator density)))))
	(if (functionp offset)
	    (hist-iter-over histo #'inc-fn :use-index t)
	    (hist-iter-over histo #'inc    :use-index t)))
      (list (float (+ (* (/ x-numerator denominator) tick) min))
	    (float (+ (* (/ y-numerator denominator) y-tick) y-min))))))

(defmethod hist-normed-reduce ((histo 2d-histogram)
			       &key norm-fn offset min max bins)
  (let ((reduced-hist (make-histogram '()
				      :min  min
				      :max  max
				      :bins bins))
	(x-offset     0.5)
	(y-offset     0.5))
    ;; update offset for constant offset
    (case offset
      (:left   (setf x-offset 0.0
		     y-offset 0.0))
      (:right  (setf x-offset 1.0
		     y-offset 1.0))
      (:center (setf x-offset 0.5
		     y-offset 0.5))
      (otherwise
       (cond ((listp offset)
	      (destructuring-bind (x-o y-o) offset
		(setf x-offset (if (numberp x-o) x-o 0.5)
		      y-offset (if (numberp y-o) y-o 0.5))))
	     ((numberp offset)
	      (setf x-offset offset
		    y-offset offset)))))
    ;; add normed to histogram
    (flet ((inc (count x-left y-bottom x-right y-top)
	     (let* ((x (+ (* x-offset x-left)
			  (* (- 1.0 x-offset) x-right)))
		    (y (+ (* y-offset y-bottom)
			  (* (- 1.0 x-offset) y-top)))
		    (norm (funcall norm-fn (list x y))))
	       (dotimes (i count)
		 (add-to-hist reduced-hist norm))))
	   (inc-fn (count x-left y-bottom x-right y-top)
	     (let* ((vector (funcall offset x-left y-bottom x-right y-top))
		    (norm   (funcall norm-fn vector)))
	       (dotimes (i count)
		 (add-to-hist reduced-hist norm)))))
      (if (functionp offset)
	  (hist-iter-over histo #'inc-fn :use-index nil)
	  (hist-iter-over histo #'inc    :use-index nil)))
    ;; return reduced normed histogram
    reduced-hist))

(defmethod hist-normed-mean ((histo 2d-histogram)
			     &key (norm-fn #'hist-vector-euclid-norm)
			       (offset :center)
			       (density-fn #'identity))
  (let ((numerator    0.0)
	(denominator  0.0)
	(x-offset     0.5)
	(y-offset     0.5))
    ;; update offset for constant offset
    (case offset
      (:left   (setf x-offset 0.0
		     y-offset 0.0))
      (:right  (setf x-offset 1.0
		     y-offset 1.0))
      (:center (setf x-offset 0.5
		     y-offset 0.5))
      (otherwise
       (cond ((listp offset)
	      (destructuring-bind (x-o y-o) offset
		(setf x-offset (if (numberp x-o) x-o 0.5)
		      y-offset (if (numberp y-o) y-o 0.5))))
	     ((numberp offset)
	      (setf x-offset offset
		    y-offset offset)))))
    ;; add normed to histogram
    (flet ((inc (count x-left y-bottom x-right y-top)
	     (let* ((x (+ (* x-offset x-left)
			  (* (- 1.0 x-offset) x-right)))
		    (y (+ (* y-offset y-bottom)
			  (* (- 1.0 x-offset) y-top)))
		    (density (funcall density-fn count))
		    (norm    (funcall norm-fn (list x y))))
	       (incf numerator   (* density norm))
	       (incf denominator density)))
	   (inc-fn (count x-left y-bottom x-right y-top)
	     (let* ((vector  (funcall offset x-left y-bottom x-right y-top))
		    (density (funcall density-fn count))
		    (norm    (funcall norm-fn vector)))
	       (incf numerator   (* density norm))
	       (incf denominator density))))
      (if (functionp offset)
	  (hist-iter-over histo #'inc-fn :use-index nil)
	  (hist-iter-over histo #'inc    :use-index nil)))
    ;; return reduced normed histogram
    (float (/ numerator denominator))))

(defmethod hist-dump-empty ((hist 2d-histogram))
  (let+ (((x-min y-min) (hist-min hist))
	 ((x-max y-max) (hist-max hist))
	 ((x-bins y-bins) (hist-bins hist)))
    (make-2d-histogram () :x-min  x-min
			  :x-max  x-max
			  :y-min  y-min
			  :y-max  y-max
			  :x-bins x-bins
			  :y-bins y-bins)))

(defmethod hist-add ((hist1 2d-histogram) (hist2 2d-histogram))
  (let ((new-hist (hist-dump-empty hist1)))
    (setf (slot-value new-hist 'buffer-hist)
	  (append (slot-value hist1 'buffer-hist)
		  (slot-value hist2 'buffer-hist)))
    (with-slots (hist) new-hist
      (let+ (((x-bins y-bins) (hist-bins new-hist)))
	(loop for x below x-bins do
	  (loop for y below y-bins do
	    (setf (at hist x y) (+ (at (slot-value hist1 'hist) x y)
				   (at (slot-value hist2 'hist) x y)))))))
    new-hist))

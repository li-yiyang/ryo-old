(in-package :ryo/statistics)

;; This file defines 1D histogram class `histogram'.

(defclass histogram ()
  ((min  :initarg :min :initform -1.0)
   (max  :initarg :max :initform  1.0)
   (tick :initform 0.02)
   (bins :initarg :bins :initform  100)
   (buffer-hist  :initform ()) ;; used for hist
   (buffer-cache :initform ()) ;; used for those not in hist, cleared by `hist-clear-buffer!'
   hist)
  (:documentation
   "A bucketed histogram.

Use `make-histogram' function to create 1D histogram from 1D list data.
It is also recommanded to use `make-histogram' even if you don't have
precollected data like this:

    (make-histogram () :min ... :max ..)

Then use `add-to-hist' function to add more data to histogram instance.

    (add-to-hist hist val)

To iter over histogram instance data, use `hist-iter-over':

    (hist-iter-over hist (lambda (count left right) ...))

where `count' is the histogram sampling count number;
`left' and `right' is the sampling bin range [left, right).

Use `hist-max', `hist-min' and `hist-bins' to access hist attributes.

Use `hist-to-csv' and `hist-to-ascii' for rendering the data.

For `histogram', the `hist-to-ascii' will return histogram plot in
vertical chart.

For developer:
the hist data is stored in an array (slot `hist'):


    #(<min bin count> -> ... -> <max bin count>)
"))

;; Develop Note:
;; below methods should be same for all `histogram' children:
;; + `add-to-hist':
;;   Shall not define `add-to-hist' behavior,
;;   define `%add-to-hist' instead
;; + `hist-clear-buffer!';
;; + `hist-clear-all!';
;; + `initialize-instance';
;; + `setf' `:after' for `hist-bins', `hist-min', `hist-max':
;;   call `hist-rebin!' after updated the hist infomation,
;;   should not call within children class method definition.
;; + `hist-rebin!' `:after': loop to resample
;;   should implement `hist-rebin!' for each children class;
;; + `hist-normed-reduce' `:around': wrap the `min', `max', `bins'
;;   arguments
;;   should implement `hist-normed-reduce' for each children class;
;; + `hist-dump'

(defmethod add-to-hist ((hist histogram) val)
  (with-slots (buffer-hist buffer-cache) hist
    (let ((indexs (multiple-value-list (%add-to-hist hist val))))
      (if (car indexs) 			; first not nil, a.k.a. have indexs returned
	  (push val buffer-hist)
	  (push val buffer-cache))
      (values-list indexs))))

(defmethod hist-clear-buffer! ((hist histogram))
  (with-slots (buffer-cache) hist
    (setf buffer-cache ())))

(defmethod hist-clear-all! ((hist histogram))
  (with-slots (buffer-hist buffer-cache) hist
    (setf buffer-cache ()
	  buffer-hist  ())
    (hist-rebin! hist)))

(defmethod initialize-instance :after ((hist histogram) &key)
  (hist-rebin! hist))

(defmethod (setf hist-bins) :after (bins (hist histogram) &rest axies)
  (declare (ignore bins axies))
  (hist-rebin! hist))

(defmethod (setf hist-min) :after (min (hist histogram) &rest axies)
  (declare (ignore min axies))
  (hist-rebin! hist))

(defmethod (setf hist-max) :after (max (hist histogram) &rest axies)
  (declare (ignore max axies))
  (hist-rebin! hist))

(defmethod hist-rebin! :after ((hist histogram))
  (with-slots (buffer-hist buffer-cache) hist
    ;; resample buffer to hist
    (loop for val in buffer-hist do
      (%add-to-hist hist val))
    (loop for val in buffer-cache do
      (%add-to-hist hist val))))

(defmethod hist-normed-reduce :around ((hist histogram)
				       &key
					 (norm-fn #'hist-vector-euclid-norm)
					 (offset  :center)
					 (min     :default)
					 (max     :default)
					 (bins    :default))
  (flet ((min* (list) (if (listp list) (reduce #'min list) list))
	 (max* (list) (if (listp list) (reduce #'max list) list)))
    (let ((min (case min
		 (:default 0.0)
		 (:outer   (hist-vector-euclid-norm (hist-min hist)))
		 (:inner   (min* (hist-min hist)))
		 (otherwise
		  (cond ((functionp min)
			 (funcall min (hist-min hist) (hist-max hist)))
			((numberp min) min)
			(t 0.0)))))
	  (max (case max
		 ((:default :outer)
		  (hist-vector-euclid-norm (hist-max hist)))
		 (:inner (min* (hist-max hist)))
		 (otherwise
		  (cond ((functionp max)
			 (funcall max (hist-min hist) (hist-max hist)))
			((numberp max) max)
			(t (hist-vector-euclid-norm (hist-max hist)))))))
	  (bins (case bins
		  (:default (max* (hist-bins hist)))
		  (otherwise
		   (cond ((functionp bins)
			  (funcall bins (hist-bins hist)))
			 ((numberp bins) bins)
			 (t (max* (hist-bins hist))))))))
      (call-next-method hist
			:norm-fn norm-fn
			:bins    bins
			:min     min
			:max     max
			:offset  offset))))

;; By default, `hist-shape-eq' should return `nil'

(defmethod hist-shape-eq (hist1 hist2)
  (declare (ignore hist1 hist2))
  nil)

;; Wrap around the `hist-add' to first test if two hist is in same shape.
;; define `hist-add' method for specific methods

(defmethod hist-add :around (hist1 hist2)
  (if (hist-shape-eq hist1 hist2)
      (call-next-method)
      (error (format nil "~A and ~A should be two histogram in same shape. "
		     hist1 hist2))))

(defmethod hist-dump ((hist histogram))
  (with-slots (buffer-hist) hist
    (let ((dump (hist-dump-empty hist)))
      (loop for elem in buffer-hist do
	(add-to-hist dump elem)))))

;; Developer Note:
;; A `histogram' children should implement below methods and functions:
;; + `make-*-histogram': to make a `*-histogram' children;
;; + `hist-bins': to return histogram bins by axies;
;; + `hist-min', `hist-max': return min/max by axies;
;; + `hist-rebin!': check the min, max;
;; + `%add-to-hist': add data to hist;
;; + `hist-iter-over', `hist-to-csv', `hist-to-ascii'

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

(defmethod (setf hist-bins) (bins (hist histogram) &rest axies)
  (declare (ignore axies))
  (let ((bins (truncate (if (listp bins) (first bins) bins))))
    (setf (slot-value hist 'bins) bins)))

(defmethod hist-min ((hist histogram) &rest axies)
  (declare (ignore axies))
  (slot-value hist 'min))

(defmethod (setf hist-min) (min (hist histogram) &rest axies)
  (declare (ignore axies))
  (let ((min (if (listp min) (first min) min)))
    (setf (slot-value hist 'min) min)))

(defmethod hist-max ((hist histogram) &rest axies)
  (declare (ignore axies))
  (slot-value hist 'max))

(defmethod (setf hist-max) (max (hist histogram) &rest axies)
  (declare (ignore axies))
  (let ((max (if (listp max) (first max) max)))
    (setf (slot-value hist 'max) max)))

(defmethod hist-rebin! ((histo histogram))
  (with-slots (min max tick bins hist buffer-hist buffer-cache) histo
    (let* ((%min (min max min))
	   (%max (max max min)))
      ;; clear hist and recalculate hist range
      (setf min %min
	    max %max
	    tick (float (/ (- %max %min) bins))
	    hist (make-array bins :initial-element 0
				  :element-type 'unsigned-byte))
      ;; return histo self
      histo)))

(defmethod %add-to-hist ((histo histogram) val)
  (with-slots (hist min max tick) histo
    (when (and (<= min val) (< val max))
      (let ((index (floor (/ (- val min) tick))))
	(incf (aref hist index))
	;; return added index
	(values index)))))

(defmethod hist-iter-over ((histo histogram) fn &key (use-index t))
  (with-slots (hist bins min tick) histo
    (if use-index
	(loop for i below bins do
	  (funcall fn (aref hist i) i))
	(loop for i below bins do
	  (funcall fn (aref hist i) (+ min (* tick i)) (+ min (* tick (1+ i))))))))

(defmethod hist-to-csv ((histo histogram) path &key (if-exists :supersede))
  (with-slots (hist bins) histo
    (with-open-file (csv path :direction :output :if-exists if-exists)
      (hist-iter-over hist
		      (lambda (count x)
			(format csv "~d, ~d" x count))
		      :use-index t))))

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

(defmethod hist-mean ((histo histogram) &key (density-fn #'identity) (offset :center))
  (with-slots (hist min tick) histo
    (let ((numerator   0)
	  (denominator 0)
	  (offset (case offset
		    (:left 0.0)
		    (:right 1.0)
		    (:center 0.5)
		    (otherwise
		     (if (numberp offset) offset 0.5)))))
      (flet ((inc (count i)
	       (let ((density (funcall density-fn count)))
		 (incf numerator   (* density (+ i offset)))
		 (incf denominator density)))
	     (inc-fn (count i)
	       (let ((density (funcall density-fn count)))
		 (incf numerator   (* density (funcall offset i)))
		 (incf denominator density))))
	(if (functionp offset)
	    (hist-iter-over histo #'inc-fn :use-index t)
	    (hist-iter-over histo #'inc    :use-index t)))
      (float (+ (* (/ numerator denominator) tick) min)))))

(defmethod hist-normed-reduce ((histo histogram)
			       &key norm-fn offset min max bins)
  (let ((reduced-hist (make-histogram '()
				      :min  min
				      :max  max
				      :bins bins))
	(offset (case offset
		  (:left     0.0)
		  (:right    1.0)
		  (:center   0.5)
		  (otherwise (if (numberp offset) offset 0.5)))))
    (flet ((inc (count x-left x-right)
	     (let* ((x (+ (* offset x-left) (* (- 1.0 offset) x-right)))
		    (norm (funcall norm-fn x)))
	       (dotimes (i count)
		 (add-to-hist reduced-hist norm))))
	   (inc-fn (count x-left x-right)
	     (let* ((x (funcall offset x-left x-right))
		    (norm (funcall norm-fn x)))
	       (dotimes (i count)
		 (add-to-hist reduced-hist norm)))))
      (if (functionp offset)
	  (hist-iter-over histo #'inc-fn :use-index nil)
	  (hist-iter-over histo #'inc    :use-index nil)))
    reduced-hist))

(defmethod hist-normed-mean (histo &key (norm-fn #'hist-vector-euclid-norm)
				     (density-fn #'identity)
				     (offset :center))
  (let ((numerator   0)
	(denominator 0)
	(offset (case offset
		  (:left     0.0)
		  (:right    1.0)
		  (:center   0.5)
		  (otherwise (if (numberp offset) offset 0.5)))))
    (flet ((inc (count x-left x-right)
	     (let* ((x       (+ (* offset x-left)
				(* (- 1.0 offset) x-right)))
		    (norm    (funcall norm-fn x))
		    (density (funcall density-fn count)))
	       (incf numerator   (* density norm))
	       (incf denominator density)))
	   (inc-fn (count x-left x-right)
	     (let* ((x       (funcall offset x-left x-right))
		    (norm    (funcall norm-fn x))
		    (density (funcall density-fn count)))
	       (incf numerator   (* density norm))
	       (incf denominator density))))
      (if (functionp offset)
	  (hist-iter-over histo #'inc-fn :use-index nil)
	  (hist-iter-over histo #'inc    :use-index nil)))
    (float (/ numerator denominator))))

(defmethod hist-shape-eq ((hist1 histogram) (hist2 histogram))
  (and (= (slot-value hist1 'min) (slot-value hist2 'min))
       (= (slot-value hist1 'max) (slot-value hist2 'max))
       (= (slot-value hist1 'bins) (slot-value hist2 'bins))))

(defmethod hist-dump-empty ((hist histogram))
  (make-histogram () :min  (hist-min hist)
		     :max  (hist-max hist)
		     :bins (hist-bins hist)))

(defmethod hist-add ((hist1 histogram) (hist2 histogram))
  (let ((new-hist (hist-dump-empty hist1)))
    (setf (slot-value new-hist 'buffer-hist)
	  (append (slot-value hist1 'buffer-hist)
		  (slot-value hist2 'buffer-hist)))
    (with-slots (hist) new-hist
      (loop for i below (hist-bins new-hist) do
	(setf (at hist i) (+ (at (slot-value hist1 'hist) i)
			     (at (slot-value hist2 'hist) i)))))
    new-hist))

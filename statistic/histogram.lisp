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
  (:documentation
   "Add `val' to `histogram'.
Return values are index to hist if success, otherwise return `nil'.

Example:

    (add-to-hist hist 1)          ; will return 0 (for example) as index
    (add-to-hist hist (list x y)) ; will return values 0 0 as index

If the added infomation is beyond histogram range,
the `val' will not be sampled. However, it will still be stored
in histogram buffer until cleaned by `hist-clear-buffer!'.

To reuse those un-sampled, use `hist-rebin!' after changing the
histogram sampling range.
"))

(defgeneric %add-to-hist (hist val)
  (:documentation
   "Add `val' to `hist' hist slot.
Return values are index to hist.

Note: this function is what's behind the `add-to-hist'.
Modify this function for functionalities instead of `add-to-hist'. "))

(defgeneric hist-rebin! (hist)
  (:documentation
   "Rebin `hist'.

This should (and will) be called after changing histogram size. "))

(defgeneric hist-clear-buffer! (hist)
  (:documentation
   "Clear `hist' buffer.

Note: this will also empty histogram sampled data,
making the `hist' to a empty histogram. "))

(defgeneric hist-to-csv (hist path &key if-exists)
  (:documentation "Write `hist' to CSV file to `path'. "))

(defgeneric hist-to-ascii (hist &key stream &allow-other-keys)
  (:documentation "Output `hist' to `stream' in ASCII form. "))

(defgeneric hist-iter-over (hist fn &key use-index &allow-other-keys)
  (:documentation
   "Iterate `fn' on `hist' data.

The `fn' shall be called with hist count, range info, and so on.
It is recommanded to use flexible lambda list like `&rest' to
capture and throw away unwanted data.

Example:

    (hist-iter-over hist (lambda (count &rest args)
                           (declare (ignore args))
                           (format count)))

If `use-index' is set to be non-nil, it will use index
rather than range infomation for iter-fn.
Example:

    (hist-iter-over hist (lambda (count idx) ...)
                    :use-index t)

    (hist-iter-over hist (lambda (count left right ...) ...)
                    :use-index nil)
"))

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
      (let ((index (floor (/ (- val min) tick))))
	(incf (aref hist index))
	;; return added index
	index))))

;; Shall not define `add-to-hist' behavior, define `%add-to-hist' instead

(defmethod add-to-hist :before ((hist histogram) val)
  (push val (slot-value hist 'buffer)))

(defmethod add-to-hist ((hist histogram) val)
  (%add-to-hist hist val))

(defmethod hist-clear-buffer! ((hist histogram))
  (setf (slot-value hist 'buffer) ())
  (hist-rebin! hist))

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
	(let ((x (floor (/ (- x min) tick)))
	      (y (floor (/ (- y y-min) y-tick))))
	  (incf (aref hist y x))
	  (values x y))))))

(defmethod hist-iter-over ((histo 2d-histogram) fn &key (use-index t))
  (with-slots (hist bins y-bins min y-min tick y-tick) histo
    (if use-index
	(loop for y below y-bins do
	  (loop for x below bins do
	    (funcall fn (aref hist y x) y x)))
	(loop for y below y-bins do
	  (loop for x below bins do
	    (funcall fn (aref hist y x)
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

(defclass histogram-add-info (histogram)
  ((info-fn :initform (error "Missing `info-fn'. ") :initarg :info-fn)
   info)
  (:documentation
   "A bucketed histogram with additional infomation.

Use `make-histogram-add-info' function to create 1D histogram from 1D list data.
The `info-fn' should seperate the data for sampling and the data for storing,
and return them in values form:

    (make-histogram-add-info data (lambda (val) (values val val)))

For developer:
the info is isomorphism to hist (slot `hist'), instead, the info (array)
element is a list containing the addition information.
"))

(defun make-histogram-add-info (data info-fn &rest args &key (min -1.0) (max 1.0) (bins 100))
  "Make a histogram with additional infomation. "
  (declare (ignorable min max bins))
  (let ((histo (apply #'make-instance 'histogram-add-info
		      `(:info-fn ,info-fn ,@args))))
    (loop for dat in data do
      (add-to-hist histo dat))
    histo))

(defmethod hist-rebin! :around ((histo histogram-add-info))
  (with-slots (info bins) histo
    (setf info (make-array bins :element-type 'list :initial-element ()))
    (call-next-method)))

(defmethod hist-iter-over ((histo histogram-add-info) fn &key (use-index t))
  (with-slots (hist bins min tick info) histo
    (if use-index
	(loop for i below bins do
	  (funcall fn (aref hist i) i (aref info i)))
	(loop for i below bins do
	  (funcall fn (aref hist i)
		   (+ min (* tick i)) (+ min (* tick (1+ i)))
		   (aref info i))))))

(defmethod %add-to-hist :around ((histo histogram-add-info) val)
  (with-slots (info-fn info) histo
    (multiple-value-bind (hist-val info-val)
	(funcall info-fn val)
      (let ((index (call-next-method histo hist-val)))
	(when index
	  (push info-val (aref info index)))))))

(defmacro info-fn (&rest info-fn*)
  "Quickly generate a info-fn for `histogram-add-info'.

Example:

    (info-fn first second :info any-fn)

will be expanded into

    (lambda (val)
      (values (list (first val) (second val))
              (any-fn val)))

also, if provided is integer number:

    (info-fn 1 :info 2)

will be expanded into

    (lambda (val)
      (values (list (nth 1 val))
              (nth 2 val)))

if provided is a list, will be treated as a calling chain:

    (info-fn (first truncate) :info identity)

will be expanded into

    (lambda (val) (values (truncate (first val)) (identity val)))

If not providing with `:info', it will be function `identity' by default;
If only provide one function in each part (hist and info), the return
form will be atom, otherwise, it will be a list.
"
  (let ((val (gensym "VAL")))
    (multiple-value-bind (hist-fn info-fn)
	(labels ((rule-map (fn*)
		   (cond ((numberp fn*) `(nth ,fn* ,val))
			 ((atom    fn*) `(,fn* ,val))
			 ((endp fn*)    val)
			 (t             `(,(car fn*) ,(rule-map (rest fn*)))))))
	  ;; seperate hist-fn and info-fn, while using `rule-map' to
	  ;; map them to true function calling form.
	  (loop for info-fns on info-fn*
		for info-fn = (car info-fns)
		if (eq info-fn :info)
		  return (values hist-fn (mapcar #'rule-map (rest info-fns)))
		else collect (rule-map info-fn) into hist-fn
		finally (return (values hist-fn '()))))
      `(lambda (,val)
	 (values ,(cond ((= (length hist-fn) 0) `(identity ,val))
			((= (length hist-fn) 1) (first hist-fn))
			(t `(list ,@hist-fn)))
		 ,(cond ((= (length info-fn) 0) `(identity ,val))
			((= (length info-fn) 1) (first info-fn))
			(t `(list ,@info-fn))))))))

(defclass 2d-histogram-add-info (2d-histogram)
  ((info-fn :initform (error "Missing `info-fn'. ") :initarg :info-fn)
   info)
  (:documentation
   "A bucketed 2d-histogram with additional infomation.

Use `make-2d-histogram-add-info' function to create 1D histogram from 1D list data.
The `info-fn' should seperate the data for sampling and the data for storing,
and return them in values form:

    (make-histogram-add-info data (lambda (val) (values val val)))

For developer:
the info is isomorphism to hist (slot `hist'), instead, the info (array)
element is a list containing the addition information.
"))

(defun make-2d-histogram-add-info (data info-fn &rest args
				   &key (x-min -1.0) (x-max 1.0) (x-bins 100)
				     (y-min -1.0) (y-max 1.0) (y-bins 100))
  "Make a histogram with additional infomation. "
  (declare (ignorable x-min x-max x-bins y-min y-max y-bins))
  (let ((histo (apply #'make-instance '2d-histogram-add-info
		      `(:info-fn ,info-fn ,@args))))
    (loop for dat in data do
      (add-to-hist histo dat))
    histo))

(defmethod hist-rebin! :around ((histo 2d-histogram-add-info))
  (with-slots (info bins y-bins) histo
    (setf info (make-array (list bins y-bins) :element-type 'list :initial-element ()))
    (call-next-method)))

(defmethod hist-iter-over ((histo 2d-histogram-add-info) fn &key (use-index t))
  (with-slots (hist bins y-bins min y-min tick y-tick info) histo
    (if use-index
	(loop for y below y-bins do
	  (loop for x below bins do
	    (funcall fn (aref hist y x) x y (aref info y x))))
	(loop for y below y-bins do
	  (loop for x below bins do
	    (funcall fn (aref hist y x)
		     (+ min (* tick x)) (+ y-min (* y-tick y))
		     (+ min (* tick (1+ x))) (+ y-min (* y-tick (1+ y)))
		     (aref info y x)))))))

(defmethod %add-to-hist :around ((histo 2d-histogram-add-info) val)
  (with-slots (info-fn info) histo
    (multiple-value-bind (hist-val info-val)
	(funcall info-fn val)
      (multiple-value-bind (x y)
	  (call-next-method histo hist-val)
	(when x
	  (push info-val (aref info y x)))))))

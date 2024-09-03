(in-package :ryo/statistics)

;; This file defines the Protocol of Histogram-like class.
;; In theory, these protocol are those exposed for using outside
;; ryo.statistics package.
;;
;; Develop Note:
;; each histogram class should provide a function like:
;; `make-histogram' for `histogram' and `make-2d-histogram' for `2d-histogram'.

(defgeneric hist-bins (hist &rest axies)
  (:documentation
   "Get bin number of given `hist' on axies.

If not given `axies' information:
+ For 1d histogram, return bin number;
+ For 2d histogram, return (x-bins y-bins);
+ For n-dimensional histogram, return a list of dimensions.

If given `axies' information:
Return given axies bin number.
And if axies is not found, ignore axies information. "))

(defgeneric (setf hist-bins) (bins hist &rest axies)
  (:documentation
   "Set the bin number of given `hist' on axies.

The `bins' should in same dimension of `axies'.
If not given `axies' information:
+ For 1d histogram, `bins' should be a integer number of a list longer than 1;
+ For 2d histogram, `bins' should be a integer number or a list longer than 2;
+ For n-dimensional histogram, `bins' should be a integer number or a list longer than n.

if `bins' is a single integer number, it whould set all the axies
with same `bins'.

If given `axies' information:
The `bins' should map one to one with the axies index. "))

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

(defgeneric (setf hist-min) (min hist &rest axies)
  (:documentation
   "Set the min range of given `hist' on axies.

The `min' should in same dimension of `axies'.
If not given `axies' information:
+ For 1d histogram, `min' should be a integer number of a list longer than 1;
+ For 2d histogram, `min' should be a integer number or a list longer than 2;
+ For n-dimensional histogram, `min' should be a integer number or a list longer than n.

if `min' is a single integer number, it whould set all the axies
with same `min'.

If given `axies' information:
The `min' should map one to one with the axies index. "))

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

(defgeneric (setf hist-max) (max hist &rest axies)
  (:documentation
   "Set the max range of given `hist' on axies.

The `max' should in same dimension of `axies'.
If not given `axies' information:
+ For 1d histogram, `max' should be a integer number of a list longer than 1;
+ For 2d histogram, `max' should be a integer number or a list longer than 2;
+ For n-dimensional histogram, `max' should be a integer number or a list longer than n.

if `max' is a single integer number, it whould set all the axies
with same `max'.

If given `axies' information:
The `max' should map one to one with the axies index. "))

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
Return values are within? + index* to hist.

Example:

    (add-to-hist hist 1) ; => 1, 0

Note: this function is what's behind the `add-to-hist'.
Modify this function for functionalities instead of `add-to-hist'. "))

(defgeneric hist-rebin! (hist)
  (:documentation
   "Rebin `hist'.

This should (and will) be called after changing histogram size. "))

(defgeneric hist-clear-buffer! (hist)
  (:documentation
   "Clear `hist' buffer.

Note: this will only clear the buffer, which stores data
that are not sampled but cached when `add-to-hist'. "))

(defgeneric hist-clear-all! (hist)
  (:documentation
   "Clear all in `hist', including buffer and hist data. "))

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

For N-dimensional other than `histogram',
Example:

    (hist-iter-over 2d-hist
                    (lambda (count x-left y-bottom x-right y-top) ...)
                    :use-index nil)
"))

(defgeneric hist-mean (hist &key density-fn offset)
  (:documentation
   "Return mean value based on the density distribution.

The `density-fn' (denoted as f) will be used to calcuate the average mean:

    Σ f(hist(qi*)) * qi / Σ f(hist(qi*)) -> for < qi >.

The `offset' specifics how the datapoint is offset:
+ `:center' (default and fallback value):
  use center point for the data point: qi -> idx + 0.5;
+ `:left': use left edge for the data point: qi -> idx;
+ `:right': use right edge for the data point: qi -> idx + 1;
+ if provided with a number or a N-d list (values should within 0-1),
  use it as index offset from left: qi -> idx + offset;
+ if provided with a function, it use index to call on the offset:
  qi -> (funcall offset idx);
+ otherwise, use fallback value `:center'.

For different histogram:
+ 1d histogram: return a value for mean;
+ 2d histogram: return a mean point as list (<x> <y>);
+ n-dimensional histogram: return a mean n-d point as list.

By default, the `density-fn' is `identity' function,
which means for the mass center of the histogram distribution.

User Note:
If you found histogram mean may not provide exact result as it should,
a possible reason is that the histogram sampling loss the infomation:

Example:

    (hist-mean (make-histogram '(1 2 3 4 5) :min -1.0 :max 10.0 :bins 10) :offset offset)
    ;; => 2.8500001 if offset = :center
    ;; => 2.3000002 if offset = :left
    ;; => 3.4       if offset = :right

But you could increase the sampling bins of have a more proper
[min, max) range to make the mean more reasonable.

Also, increase the sampling data size would also be a good choice.
"))

(defgeneric hist-normed-reduce (hist &key norm-fn offset min max bins)
  (:documentation
   "Reduce the `hist' dimension to 1D histogram with `norm-fn'.
Return `histogram' instance as reduced histogram.

It will iter over `hist' and use `norm-fn' as new data for
the reduced 1D histogram.

Example:

  ;; Make RDF from `2d-histogram' on Euclid Space

  (hist-normed-reduce 2d-hist :norm-fn #'hist-vector-euclid-norm)

The `offset' are different in `hist-mean' if it is passed in
as the function to call. The `offset' function will be called
with the real coordinate as args.

The `min', `max' and `bins', could be a new assigned value,
or could be described as below:
+ `min':
  + `:default' -> set to zero;
  + `:outer' -> (hist-vector-euclid-norm (hist-min hist))
  + `:inner' -> (reduce #'min (hist-min hist))
  + a function with lambda list (mins maxs)
+ `max':
  + `:default' and `:outer' -> (hist-vector-euclid-norm (hist-max hist))
  + `:inner' -> (reduce #'min (hist-max hist))
  + a function with lambda list (mins maxs)
+ `bins':
  + `:default' -> (reduce #'max (hist-bins hist))
  + a function with lambda list (bins)

Developer Note:
+ the reduce wrapper should be defined in method `:around'.
 "))

(defgeneric hist-normed-mean (hist &key norm-fn density-fn offset)
  (:documentation
   "Return mean value based on the density distribution on normed space.

It is like `hist-mean' but apply `norm-fn' on vector {qi} before
using {qi} for calculating mean value. The `norm-fn' takes in the
coordinate as arguments, it should return a number as norm value.

Example:

    (hist-normed-mean hist :norm-fn (lambda (x y) (sqrt (+ (* x x) (* y y)))))

for Euclid space norm.

By default, the `norm-fn' is `hist-vector-euclid-norm', which is
same as Euclid norm.

The `offset' and `density-fn' are same like `hist-mean'. "))

(defgeneric hist-shape-eq (hist1 hist2)
  (:documentation
   "Test if two histogram `hist1' and `hist2' has same shape.

If `hist1' and `hist2' is in same shape, they have to be in same:
+ range: their sampling range should be same;
+ bins: their sampling bins should be same. "))

(defgeneric hist-add (hist1 hist2)
  (:documentation
   "Add two histogram with same dimension.
Return a new histogram with added hist count.

If two histogram is addable, they have to be in same shape,
which is detected by `hist-shape-eq'. "))

(defgeneric hist-dump-empty (hist)
  (:documentation
   "Make a new empty histogram with same shape like `hist'.
Return a new empty histogram. "))

(defgeneric hist-dump (hist)
  (:documentation
   "Make a new histogram with same data as `hist'.
Return a new dumped histogram.

Note this is almost same like copy a histogram and do
`hist-clear-buffer!' on the copied histogram, which
means copied histogram does not include `buffer-cache'. "))

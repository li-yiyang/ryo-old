(in-package :ryo.statistics)

;; This file defines 2D histogram with additional infomation. 

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

(in-package :ryo/statistics)

;; This file defines histogram with additional information.

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

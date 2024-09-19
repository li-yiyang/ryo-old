(in-package :ryo/tex)

(defclass container (widget)
  ((children :initform (make-array 0 :fill-pointer 0
				     :adjustable t)))
  (:documentation
   "The basic container (environment like) element. "))

(defmethod add-to ((widget widget) (container container))
  (vector-push-extend widget (slot-value container 'children))
  (setf (slot-value widget 'parent) container)
  widget)

(defmethod render ((container container))
  (loop for widget across (slot-value container 'children)
	do (format t "~&")
	do (render widget)))


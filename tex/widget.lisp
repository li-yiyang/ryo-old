(in-package :ryo/tex)

(defparameter *container* nil
  "The default container of LaTeX element. ")

(defclass widget ()
  ((parent   :initarg    :parent
	     :initform   *container*
	     :reader     parent)
   (id       :initform   nil
	     :reader     id)
   (link-p   :initform   nil
	     :reader     link-p)
   (tex-name :initform   ""
	     :reader     tex-name
	     :allocation :class)
   (title-p  :initform   nil
	     :reader     title-p
	     :allocation :class)
   (author-p :initform   nil
	     :reader     author-p
	     :allocation :class)
   (linkable :initform   nil
	     :reader     linkable
	     :allocation :class))
  (:documentation
   "The basic LaTeX element. "))

(defmethod initialize-instance :after ((widget widget) &key)
  (with-slots (id) widget
    (setf id (gensym (symbol-name (class-name (class-of widget)))))))

(defmethod root-p ((widget widget))
  (if (parent widget) t nil))

(defmethod root ((widget widget))
  (if (root-p widget)
      widget
      (root (parent widget))))

(defmethod title-of ((widget widget))
  (if (root-p widget) nil (title-of (parent widget))))

(defmethod (setf title-of) (title (widget widget))
  (if (root-p widget)
      (warn (format nil "Failed to set title for root ~A. " widget))
      (setf (title-of (parent widget)) title)))

(in-package :ryo/tex)

(defclass link (widget)
  ((link-to :initform nil
	    :initarg  :link-to))
  (:documentation
   "A `link' object. "))



(in-package :ryo/tex)

(defclass image (widget)
  ((title-p :initform t)
   (title   :initform ""
	    :initarg  :title
	    :reader   title-of)
   (path    :initform (error "Missing image `path'. ")
	    :initarg  :path)
   ;; (width   :initform :auto
   ;; 	    :initarg  :width)
   ;; (height  :initform :auto
   ;; 	    :initarg  :height)
   )
  (:documentation
   "The image container. "))

(defun image (path &key (caption "") &allow-other-keys)
  "Create an `image' and insert it to the `*container*'. "
  (let ((image (make-instance 'image :path path :title caption)))
    (add-to image *container*)))

(defmethod render ((image image))
  (let ((path  (uiop:native-namestring (slot-value image 'path)))
	(title (title-of image)))
    (tex-env ("figure" "htbp")
      (tex-cmd "centering")
      (tex-cmd (list "includegraphics" "width=0.618\\textwidth") path)
      (when-str title (tex-cmd "caption" title)))))

(in-package :ryo/tex)

(defclass environment (container) ()
  (:documentation
   "The template class for LaTeX environment. "))

(defmethod render :around ((env environment))
  (let ((name (tex-name env)))
    (tex-env name (call-next-method))))

(defmacro define-environment (name &key (direct-slots ())
				     (before nil before-p)
				     (after  nil after-p)
				     (key-hints ())
				     (tex-name  (mk-tex-name name) tex-name-p)
				     (documentation (format nil "LaTeX environment ~A" name)))
  "Define a LaTeX environment. "
  (let ((direct-slots (cond ((and (assoc 'tex-name direct-slots) tex-name-p)
			     (warn "Omitted `:tex-name' key for directly set `tex-name' in `direct-slots'. ")
			     direct-slots)
			    (t (cons `(tex-name :initform ,tex-name)
				     direct-slots)))))
    `(progn
       ;; define environment class
       (defclass ,name (environment)
	 (,@direct-slots)
	 (:documentation ,documentation))

       ;; if `before-p' and `after-p', define the render :before and :end method
       ,(when before-p
	  `(defmethod render :before ((env ,name))
	     (funcall ,before env)))
       ,(when after-p
	  `(defmethod render :after ((env ,name))
	     (funcall ,after  env)))

       ;; define DSL for creating the environment
       (defmacro ,name ((&rest args &key ,@key-hints &allow-other-keys) &body body)
	 ,(when key-hints `(declare (ignore ,@key-hints)))
	 `(let ((env (make-instance ',',name ,@args)))
	    (add-to env *container*)
	    (let ((*container* env))
	      ,@body
	      *container*))))))

(define-environment frame
  :direct-slots ((title    :initform ""
			   :initarg  :title
			   :accessor title-of)
		 (title-p  :initform t)
		 (subtitle :initform ""
			   :initarg  :subtitle
			   :accessor subtitle-of))
  :key-hints (title subtitle)
  :before (lambda (frame)
	    (let ((title    (title-of    frame))
		  (subtitle (subtitle-of frame)))
	      (when-str title    (tex-cmd "frametitle"    title))
	      (when-str subtitle (tex-cmd "framesubtitle" subtitle))))
  :tex-name "frame")

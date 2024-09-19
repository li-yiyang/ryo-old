(in-package :ryo/tex)

(defparameter *default-package*
  '("hyperref" "xeCJK" "capt-of" "graphicx")
  "The default package. ")

(defclass document (container)
  ((usepackage    :initarg  :usepackage
		  :initform *default-package*)
   (title         :initarg  :title
		  :initform nil
		  :accessor title-of)
   (title-p       :initform t)
   (author        :initarg  :author
		  :initform nil)
   (author-p      :initform t))
  (:documentation
   "The template class for LaTeX documentation.

To initialize the `document' class instance,
The `:usepackage' and `:package' shall be a list of:
+ a string: for the package name string;
+ a list of string: the first is the package name string,
  and the rest shall be the package parameters, each will
  be splited by a command when rendering into LaTeX. "))

(defmethod render :around ((doc document))
  (with-slots (usepackage title author) doc
    ;; \documentclass{}
    (tex-cmd "documentclass" (tex-name doc))

    ;; \usepackage{}
    (loop for package in usepackage
	  if (listp package)
	    do (tex-cmd (cons "usepackage" (rest  package))
			(first package))
	  else do (tex-cmd "usepackage" package))

    (when-str title  (tex-cmd "title"  title))
    (when-str author (tex-cmd "author" author))

    (tex-env "document" (call-next-method))))

(defmacro define-document (name &key (default-package '*default-package*)
				  (more-package ())
				  (key-hints '(title author))
				  (tex-name nil tex-name-p)
				  (before nil before-p)
				  (documentation (format nil "LaTeX document class ~A" name)))
  "Define a LaTeX document class.

The `before' shall be in lambda form, which will called before rendering
the body of document. "
  `(progn
     ;; define document class
     (defclass ,name (document)
       ((usepackage :initform (append ,default-package ,more-package))
	(tex-name   :initform ,(if tex-name-p tex-name (format nil "~(~A~)" name))))
       (:documentation ,documentation))

     ;; if `before-p', define the render :before method
     ,(when before-p
	`(defmethod render :before ((doc ,name))
	   (funcall ,before doc)))

     ;; define DSL for creating the document
     (defmacro ,name ((&rest args &key ,@key-hints &allow-other-keys) &body body)
       ,(when key-hints `(declare (ignore ,@key-hints)))
       `(let ((*container* (make-instance ',',name ,@args)))
	  ,@body
	  *container*))))

(define-document article)
(define-document beamer)

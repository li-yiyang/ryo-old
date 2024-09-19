(in-package :ryo/tex)

(defgeneric parent (widget)
  (:documentation
   "Get the parent of `widget'. "))

(defgeneric id (widget)
  (:documentation
   "Get the id of `widget'. "))

;; (defgeneric link-p (widget)
;;   (:documentation
;;    "Whether a `widget' is linked.

;; For a linked `widget', it will be appended with a label mark
;; when rendered into LaTeX.

;; Use `link-to' will create a link to the `widget'. "))

;; (defgeneric linkable (widget)
;;   (:documentation
;;    "Whether a `widget' is `linkable'. "))

;; (defgeneric link-to (widget &optional description)
;;   (:documentation
;;    "Create a `link' object to `widget' with `description'.

;; The `description' shall be a string to display the hyperlink.
;; If not set `description', it will be automatically generated. "))

(defgeneric root (widget)
  (:documentation
   "Get the root of `widget'. "))

(defgeneric root-p (widget)
  (:documentation
   "Test if the `widget' is root widget. "))

(defgeneric render (widget)
  (:documentation
   "Render `widget' as LaTeX string. "))

(defgeneric tex-name (widget)
  (:documentation
   "Return the LaTeX name of `widget'. "))

(defgeneric add-to (widget container)
  (:documentation
   "Add `widget' to `container' as children.
Return the added `widget'. "))

(defgeneric title-p (widget)
  (:documentation
   "Whether a widget is able to set title. "))

(defgeneric author-p (widget)
  (:documentation
   "Whether a widget is able to set author. "))

(defgeneric title-of (widget)
  (:documentation
   "Return the title of `widget'.
If `title-p' of the `widget' is not `t', it will try
to get the parent `title' unless the `widget' is `root-p'. "))

(defgeneric (setf title-of) (title widget)
  (:documentation
   "Set the title of `widget'.
If `title-p' of the `widget' is not `t', it will try
to set the parent `title' unless the `widget' is `root-p'. "))

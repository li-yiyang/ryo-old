(in-package :ryo/tex)

(defclass plain (widget)
  ((text :initarg  :text
	 :initform ""))
  (:documentation
   "The raw LaTeX as plain input.
Use `plain' function to create a `plain' widget. "))

(defun plain (text-or-control-string &rest args)
  "Make a `plain' object and `add-to' the `*container*'.

The `text-or-control-string' will be
+ a simple LaTeX string if no more `args' are provided;
+ a formatting string used by `format' if provided with `args'.

Example:

    (plain \"a simple \\LaTeX string\")
    (plain \"~Dth, as formatting string\" i)
"
  (let* ((text (if (endp args)
		   text-or-control-string
		   (apply #'format nil text-or-control-string args)))
	 (plain (make-instance 'plain :text text)))
    (add-to plain *container*)))

(defmethod render ((plain plain))
  (format t "~&~A" (slot-value plain 'text)))

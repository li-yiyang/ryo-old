(in-package :ryo)

(defun %parse-key-value-body (key-value-body)
  "Parse the `key-value-body' return values are plist and body.

The `key-value-body' could be in form:

   ([(:key val)+] . body)

For example:

   (multiple-value-bind (key-value body)
       (%parse-key-value-body '(:skip 3 i))
     (do-something-with key-value body))

"
  (labels ((collect (body key-value)
             (let ((elem (first body)))
               (if (keywordp elem)
                   (collect (cddr body) (cons (list elem (second body)) key-value))
                   (values (apply #'nconc key-value) body)))))
    (collect key-value-body ())))

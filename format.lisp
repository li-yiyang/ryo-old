(in-package :ryo)

(defmacro errorf (control-string &rest format-arguments)
  "Rise error with format `control-string' and `format-arguments'. "
  `(error (format nil ,control-string ,@format-arguments)))

(defmacro warnf (control-string &rest format-arguments)
  "Rise warning with format `control-string' and `format-arguments'. "
  `(warn (format nil ,control-string ,@format-arguments)))

;; The `format-table' got inspired form
;; Pretty Printing Table Data in Common Lisp
;; https://gist.github.com/WetHat/a49e6f2140b401a190d45d31e052af8f

(defun format-table (stream table
                     &key (headers () headers-set?)
                       (aligns () aligns-set?)
                       (default-align :center))
  "Format `table' to `stream'.

The table align could be `:center', `:left' or `:right'.
Set `aligns' for setting each column with specific align.
Or set `default-align' for default.

For example:

  (format-table nil '((1 2) (3 4))
                :headers '(A B))"
  (let* ((cols (length (first table)))
         (col-width (make-array (list cols)))
         (table
           (cond (headers-set?
                  (unless (length= headers cols)
                     (errorf "Table is ~d columns, but only ~d headers. "
                             cols (length headers)))
                  (cons headers table))
                 (t table)))
         (table
           (loop for row in table
                 collect (loop for col-idx from 0
                               for col in row
                               for col-str = (format nil "~a" col)
                               for col-len = (length col-str)
                               do (setf (aref col-width col-idx)
                                        (max col-len
                                             (aref col-width col-idx)))
                               collect col-str)))
         (aligns
           (cond (aligns-set?
                  (unless (length= aligns cols)
                    (errorf "Table is ~d columns, but only ~d aligns. "
                            cols (length aligns)))
                  aligns)
                 (t (make-list cols :initial-element default-align))))
         (line-control
           (loop for col below cols
                 for align in aligns
                 collect (format nil "~~~d:~a"
                                 (aref col-width col)
                                 (ecase align
                                   (:center "@<~a~>")
                                   (:left   "a")
                                   (:right  "@a")))))
         (table (loop for row in table
                      collect (loop for col in row
                                    for ctl in line-control
                                    collect (format nil ctl col)))))
    (if headers-set?
        (format stream "~&|~{ ~a ~^|~}|~%|~{-~a-~^|~}|~%~{~&|~{ ~a ~^|~}|~}"
                (first table)
                (map 'list
                     (lambda (width) (format nil "~v@{~a~:*~}" width "-"))
                     col-width)
                (rest table))
        (format stream "~{~&|~{ ~a ~^|~}|~}" table))))

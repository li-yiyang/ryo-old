(in-package :ryo/statistics)

;; This package is about how to render
;; histogram via clog-c3.

(defmacro with-output-to-strings ((&rest str-vars) progn &body body)
  `(let ,str-vars
     ,(loop for var in str-vars
            do (setf progn
                     `(setf ,var (with-output-to-string (,var)
                                   ,progn)))

            finally (return progn))
     ,@body))

(trivial-indent:define-indentation with-output-to-strings (4 2 &body))

(defmethod clog-c3:c3-form ((histo histogram))
  (with-output-to-strings (ys xs)
    (with-iter-hist (histo :use-index nil)
        (count left right)
      (format ys "~D," count)
      (format xs "~,4F," (/ (+ left right) 2.0)))
    (values ys xs)))

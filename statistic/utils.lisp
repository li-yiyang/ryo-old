(in-package :ryo.statistics)

;; This file defines some utilities functions for `ryo.statistics' package
;; to use. 

(defun hist-vector-euclid-norm (vector)
  "Vector should be a sequence like `list' or `vector' or signle number.
Return Euclid Norm of the vector via:

    euclid-norm = sqrt(Σ qi^2)

"
  (if (numberp vector) vector		; 1D number
      (sqrt (reduce #'+
		    (map 'list (lambda (qi) (* qi qi)) vector)
		    :initial-value 0.0))))

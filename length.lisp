(in-package :ryo)

;; these three macro I just copy from Elisp, which I found
;; very helpful to use. 

(defmacro length= (sequence length)
  "If length of `sequence' is equal to `length'. "
  `(= (length ,sequence) ,length))

(defmacro length> (sequence length)
  "If length of `sequence' is larger than `length'. "
  `(> (length ,sequence) ,length))

(defmacro length< (sequence length)
  "If length of `sequence' is less than `length'. "
  `(< (length ,sequence) ,length))

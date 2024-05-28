(in-package :ryo)

(defmacro at (matrix &rest indexs)
  "A more nature way to express matrix indexing.

For example:

  (at matrix i j) -> (aref matrix j i)
"
  `(aref ,matrix ,@(reverse indexs)))

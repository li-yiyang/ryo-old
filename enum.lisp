(in-package :ryo)

(defmacro defenum (name &body keyword-index-pairs)
  "Make a enumator function with `name'.

The `keyword-index-pairs' could be:

   (keyword idx): a list setting specific idx for keyword
   keyword: a symbol used to enum

For example:

   (defenum weekday
     (:monday 1) -> :monday => 1
     :tuesday    -> next is :tuesday, will be 2
     :wednesay   -> next is 3
     :thursday   -> and so on...
     :friday :saturday :sunday)

Then use it like

   (weekday 1)       ; => :monday
   (weekday :monday) ; => 1

Use KEYWORD type to make enumator.
"
  (let ((keyword-index-pairs
          (loop for key-idx in keyword-index-pairs
                for pair? = (listp key-idx)
                for index = (if pair? (second key-idx) 0)
                  then (if pair? (second key-idx) (1+ index))
                for key = (if pair? (first key-idx) key-idx)
                do (unless (keywordp key)
                     (warn
                      (format nil "Recommanded to use keyword than ~a. " key)))
                do (unless (integerp index)
                     (error
                      (format nil "Index should be integer than ~a. " index)))
                collect (list key index))))
    `(defun ,name (keyword-or-index)
       ,(format nil "Transform between keyword and index.~%Transform Table:~%~a"
                (format-table nil keyword-index-pairs :headers '(keyword index)))
       (cond ((integerp keyword-or-index)
              (ecase keyword-or-index
                ,@(loop for (key idx) in keyword-index-pairs
                        collect (list idx key))))
             ((keywordp keyword-or-index)
              (ecase keyword-or-index
                ,@(loop for (key idx) in keyword-index-pairs
                        collect (list key idx))))
             (t (errorf "~a should be keyword or index. " keyword-or-index))))))

(in-package :ryo.statistics)

;; This file defines some helper macros to use for `ryo.statistics'.

(defmacro with-axies-idx (axies &rest values)
  "Map `values' according to `axies'.

Example:

    (with-axies-idx axies x-axies y-axies)

    ;; will be expanded into:

    (mapcar (lambda (i)
              (ecase (mod i 2)
                (0 x-axies)
                (1 y-axies)))
            axies)
"
  (let ((count (length values)))
    `(mapcar (lambda (i)
	       (ecase (mod i ,count)
		 ,@(loop for i from 0
			 for val in values
			 collect (list i val))))
	     ,axies)))

(defmacro with-axies-idx-setf (axies &rest values-binding)
  "Map `values' according to `axies'.

Example:

    (with-axies-idx-setf axies x-axies y-axies)

    ;; will be expanded into:

    (mapcar (lambda (i)
              (ecase (mod i 2)
                (0 x-axies)
                (1 y-axies)))
            axies)
"
  (let ((count (length values-binding)))
    `(mapcar (lambda (i)
	       (ecase (mod i ,count)
		 ,@(loop for i from 0
			 for (var val) in values-binding
			 collect `(,i (setf ,var ,val)))))
	     ,axies)))

(defmacro hist-axies-setf (hist axies input &rest vars)
  (let ((new-vars (mapcar (lambda (var) (gensym (format nil "~@(~A~)" var))) vars))
	(length (length vars)))
    `(with-slots ,vars ,hist
       (destructuring-bind ,new-vars
	   (if (listp ,input) ,input
	       (make-list ,length :initial-element ,input))
	 (if (endp ,axies)
	     (setf ,@(loop for var in vars
			   for new-var in new-vars
			   collect var collect new-var))
	     (with-axies-idx-setf ,axies
	       ,@(loop for var in vars
		       for new-var in new-vars
		       collect (list var new-vars))))))))

;; This macro from `ryo:at' in `matrix.lisp'.
;; It is copied rather than imported for `matrix.lisp' is going to
;; be removed in the future code.

(defmacro at (matrix &rest indexs)
  "A more nature way to express matrix indexing.

For example:

  (at matrix i j) -> (aref matrix j i)
"
  `(aref ,matrix ,@(reverse indexs)))

(defmacro info-fn (&rest info-fn*)
  "Quickly generate a info-fn for `histogram-add-info'.

Example:

    (info-fn first second :info any-fn)

will be expanded into

    (lambda (val)
      (values (list (first val) (second val))
              (any-fn val)))

also, if provided is integer number:

    (info-fn 1 :info 2)

will be expanded into

    (lambda (val)
      (values (list (nth 1 val))
              (nth 2 val)))

if provided is a list, will be treated as a calling chain:

    (info-fn (first truncate) :info identity)

will be expanded into

    (lambda (val) (values (truncate (first val)) (identity val)))

If not providing with `:info', it will be function `identity' by default;
If only provide one function in each part (hist and info), the return
form will be atom, otherwise, it will be a list.
"
  (let ((val (gensym "VAL")))
    (multiple-value-bind (hist-fn info-fn)
	(labels ((rule-map (fn*)
		   (cond ((numberp fn*) `(nth ,fn* ,val))
			 ((atom    fn*) `(,fn* ,val))
			 ((endp fn*)    val)
			 (t             `(,(car fn*) ,(rule-map (rest fn*)))))))
	  ;; seperate hist-fn and info-fn, while using `rule-map' to
	  ;; map them to true function calling form.
	  (loop for info-fns on info-fn*
		for info-fn = (car info-fns)
		if (eq info-fn :info)
		  return (values hist-fn (mapcar #'rule-map (rest info-fns)))
		else collect (rule-map info-fn) into hist-fn
		finally (return (values hist-fn '()))))
      `(lambda (,val)
	 (values ,(cond ((= (length hist-fn) 0) `(identity ,val))
			((= (length hist-fn) 1) (first hist-fn))
			(t `(list ,@hist-fn)))
		 ,(cond ((= (length info-fn) 0) `(identity ,val))
			((= (length info-fn) 1) (first info-fn))
			(t `(list ,@info-fn))))))))

(defmacro let+ (bindings &body body)
  (if (endp bindings)
      `(progn ,@body)
      (let ((var-exp (first bindings)))
	`(destructuring-bind ,(first var-exp)
	     ,(second var-exp)
	   (let+ ,(rest bindings)
	     ,@body)))))

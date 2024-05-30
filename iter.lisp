(in-package :ryo)

(defmacro iter-i* (iter-range* &body key-value-body)
  "Make a iter body.

The `iter-range*' should be (var [start = 0] end [step = 1]),
if provide only end via (var end), will iter `var' from 0 to (1- end);
if provide with (var start end), will iter `var' from `start' to `end';
default step is `1'.

For example:

  (iter-i* ((i 5) (j 3 5))
    (do-something-with i j))

will be expanded to

  (loop for j from 3 to 5 by 1
      do (loop for i from 0 to 4 by 1
               do (progn (do-something-with i j))))

the last var will be iter as outer variable.

`iter-i*' could be configured in `key-value-body' form,
the key-value pair could be one of `:reject', `:accept'
or `:skip'.

`:reject' and `:accept' should provided with a test
 function whose input are iter-vars in sequence.

`:skip' should provided with a number (if only one iter-var)
or a list of number (must if for multiple iter-var),
which would be truned to `:reject' lambda function.
`:skip' only skip one specific iter-var pattern, if you
want to skip on serval patterns, use `:reject' instead. 

For example:

  (iter-i* ((i 5))
    :reject #'even                      ; only do when odd
    (do-something-with i))

if provided with `:reject' parameter, will skip on specific
iter-var condition if reject function return is non-nil.
"
  (multiple-value-bind (key-value body)
      (%parse-key-value-body key-value-body)
    (let ((reject (getf key-value :reject nil))
          (accept (getf key-value :accept nil))
          (skip   (getf key-value :skip   nil))
          (test-f (gensym "TEST-F"))
          (iter-vars (mapcar #'first iter-range*)))
      (when (or (and reject accept) (and reject skip) (and skip accept))
        (error "iter-i* should take only one of `:reject', `:accept' and `:skip'. "))
      (flet ((->range (a &optional (b 0 b-set?) (step 1))
               (if b-set? `(,a ,b ,step) `(,0 (1- ,a) ,step)))
             (make-test-f ()
               (cond (reject reject)
                     (accept accept)
                     ((listp skip)
                      (flet ((make-equal (a b) `(= ,a ,b)))
                        `(lambda ,iter-vars
                           (and ,@(mapcar #'make-equal iter-vars skip)))))
                     ((atom skip)
                      (unless (= (length iter-vars) 1)
                        (error "Single `:skip' should only with one iter-var. "))
                      `(lambda ,iter-vars
                         (= ,@iter-vars ,skip))))))
        (loop
          for program
            = `(,@(cond ((or reject skip) `(unless (funcall ,test-f ,@iter-vars)))
                        (accept           `(when   (funcall ,test-f ,@iter-vars)))
                        (t '(progn)))
                ,@body)
              then `(loop for ,var from ,start ,direct ,end by ,step
                          do ,program)
          for (var . range) in iter-range*
          for (start end step) = (apply #'->range range)
          for direct = (if (> step 0) 'to 'downto)
          finally (return (if (or accept reject skip)
                              `(let ((,test-f ,(make-test-f)))
                                 ,program)
                              program)))))))

(defmacro sum-iter-i* (iter-range* &body key-value-body)
  "Make a iter sum of the `body' results, return sum value.

The `sum-iter-i*' is like `iter-i*' but with some more keyword
`:sum-method' and `:sum-init'.

If not providing with `:sum-method' will use `+' as default
sum method, however, if setting with `:sum-method', it will
use (setf sum-var (funcall sum-method sum-var sum-add)) instead.

The `sum-add' will be equal to the last return value of body
calculation result, and like `sum-var', they are inner variable
that should not be accessed within the body.

Initially, `:sum-init' is 0 for suming up from 0.

For example:

  (sum-iter-i* ((i 5))
    :sum-method #'+
    (do-some-thing))

will be expanded into

  (let ((#:sum-var522 0))
    (iter-i* ((i 5))
      (setf #:sum-var522
            (funcall #'+ #:sum-var522 (progn (do-some-thing)))))
    #:sum-var522)

You could also use `iter-i*' configuration keys:

  (sum-iter-i* ((i 5))
    :skip 3
    (do-something-with i))

"
  (multiple-value-bind (key-value body)
      (%parse-key-value-body key-value-body)
    (let ((sum-var     (gensym "SUM-VAR"))
          (sum-method  (gensym "SUM-METHOD"))
          (method      (getf key-value :sum-method '#'+))
          (sum-init    (getf key-value :sum-init   0)))
      `(let ((,sum-var ,sum-init)
             (,sum-method ,method))
         (iter-i* ,iter-range*
           ,@key-value
           (setf ,sum-var (funcall ,sum-method ,sum-var (progn ,@body))))
         ,sum-var))))

(defmacro piter-i* (iter-range* &body key-value-body)
  "Make a parallel iter body.

The `piter-i*' use `iter-i*' to submit each parallel task
with the `lparallel' help. Behind is the `bt:make-thread' function.

PLEASE NOTE THAT the reject, accept, skip is all done in `iter-i*' part,
therefore you should NOT do some heavy test with reject, accept and skip,
otherwise you may got poor performance of parallel computation.

In a word, `piter-i*' should be used as only a Parallel task submitter.
"
  (multiple-value-bind (key-value body)
      (%parse-key-value-body key-value-body)
    (let* ((channel        (gensym "CHANNEL"))
           (channel-counts (gensym "CHANNEL-COUNTS"))
           (iter-vars (mapcar #'first iter-range*)))
      `(let ((,channel (lparallel:make-channel))
             (,channel-counts 0))
         (iter-i* ,iter-range*
           ,@key-value
           (incf ,channel-counts)
           (lparallel:submit-task ,channel (lambda ,iter-vars ,@body) ,@iter-vars))
         (dotimes (i ,channel-counts) (lparallel:receive-result ,channel))))))

(defmacro sum-piter-i* (iter-range* &body key-value-body)
  "Make a parallel sum with thread lock protect, return sum up value.

The `sum-piter-i*' is like `sum-iter-i*', but is multi-threaded by `piter-i*'.
"
  (multiple-value-bind (key-value body)
      (%parse-key-value-body key-value-body)
    (let* ((thread-lock (gensym "THREAD-LOCK"))
           (sum-add     (gensym "SUM-ADD"))
           (sum-var     (gensym "SUM-VAR"))
           (sum-method  (getf key-value :sum-method '#'+))
           (sum-init    (getf key-value :sum-init   '0)))
      `(let ((,thread-lock (bt:make-lock))
             (,sum-var ,sum-init))
         (piter-i* ,iter-range*
           ,@key-value
           (let ((,sum-add (progn ,@body)))
             (bt:with-lock-held (,thread-lock)
               (setf ,sum-var (funcall ,sum-method ,sum-var ,sum-add)))))
         ,sum-var))))

(defmacro collect-i* (iter-range* &body body)
  "Make a nested collect iter process.

The `body' result will be collected, the collect result will
be nested in sequence. If you want to collect all into a flatten
sequence, use `flat-collect-i*' instead.

The `iter-range*' of `collect-i*' is same with `iter-i*'.
HOWEVER, `collect-i*' does not support key-value-body form.
"
  (flet ((->range (a &optional (b 0 b-set?) (step 1))
           (if b-set? `(,a ,b ,step) `(,0 (1- ,a) ,step))))
    (loop for program = `(progn ,@body)
            then `(loop for ,var from ,start ,direct ,end by ,step
                        collect ,program)
          for (var . range) in iter-range*
          for (start end step) = (apply #'->range range)
          for direct = (if (> step 0) 'to 'downto)
          finally (return program))))

(defmacro flat-collect-i* (iter-range* &body key-value-body)
  "Make a flattened collect iter process.

The `body' result will be collected into a list in sequence.
If provided with `:in-order?' key with non-nil value, the
result will be in order (by default in reversed order).

Behind the scene is a `iter-i*' process and the result of `body'
will be pushed into a temp list, whose reverse was finally retruned
as results.
"
  (multiple-value-bind (key-value body)
      (%parse-key-value-body key-value-body)
    (let ((temp-list (gensym "TEMP-LIST"))
          (in-order?  (getf key-value :in-order? nil)))
      `(let ((,temp-list ()))
         (iter-i* ,iter-range*
           ,@key-value
           (push (progn ,@body) ,temp-list))
         ,(if in-order?
              `(nreverse ,temp-list)
              temp-list)))))

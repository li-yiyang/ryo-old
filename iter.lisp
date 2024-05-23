(in-package :ryo)

(defmacro iter-i* (iter-range* &body body)
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
"
  (flet ((->range (a &optional (b 0 b-set?) (step 1))
           (if b-set? `(,a ,b ,step) `(,0 (1- ,a) ,step))))
    (loop for program = `(progn ,@body)
            then `(loop for ,var from ,start ,direct ,end by ,step
                        do ,program)
          for (var . range) in iter-range*
          for (start end step) = (apply #'->range range)
          for direct = (if (> step 0) 'to 'downto)
          finally (return program))))

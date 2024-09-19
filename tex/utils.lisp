(in-package :ryo/tex)

;; this file defines some basic macros for defining the `ryo/tex' package
;; for those functionality macros, see specific files.

(defun mk-tex-name (symbol)
  "Trun `symbol' into a tex-name. "
  (format nil "~(~A~)" symbol))

(defun tex-cmd (command &rest args)
  "Format TeX command like \command[attr...]{arg}{arg} ...

If `command' is a:
+ `list': (command . attr-list)
+ otherwise: command
"
  (if (listp command)
      (format t "~&\\~A[~{~A~^,~}]~{{~A}~}"
	      (first command) (rest command) args)
      (format t "~&\\~A~{{~A}~}" command args)))

(defmacro tex-env (env &body body)
  "Format TeX environment command like \begin{env}[attr] body \end{env}

Like `tex-cmd', the `env' can be a:
+ `list': (command attr)
+ otherwise: `command'. "
  (if (listp env)
      `(progn
	 (format t "~&\\begin{~A}[~{~A~^,~}]" ,(first env) ',(rest env))
	 ,@body
	 (format t "~&\\end{~A}" ,(first env)))
      `(progn
	 (format t "~&\\begin{~A}" ,env)
	 ,@body
	 (format t "~&\\end{~A}"   ,env))))

(defmacro when-str (string &body body)
  "Ensure that the `string' is a not empty string. "
  (let ((str (gensym "STR")))
    `(let ((,str ,string))
       (when (and (stringp ,str) (not (str:emptyp ,str)))
	 ,@body))))

(defun tex (widget &key (tmp-dir (uiop:temporary-directory))
		     (tex-file nil tex-file-p)
		     (out-dir (uiop:getcwd))
		     (debug-output nil))
  "Compile the `widget' to PDF. "
  (let* ((compiler "xelatex")
	 (tex-file (if tex-file-p
		       (file-namestring tex-file)
		       (format nil "~A.tex" (id widget))))
	 (tex-name (pathname-name tex-file))
	 (pdf-name (format nil "~A.pdf" tex-name)))
    (uiop:with-current-directory (out-dir)
      ;; render widget into tex file
      (with-open-file (*standard-output* tex-file
					 :direction :output
					 :if-exists :supersede
					 :if-does-not-exist :create)
	(render widget))
      ;; output the pdf and other things to the
      (uiop:run-program `(,compiler
			  ,(format nil "--output-directory=~A"
				   (uiop:native-namestring tmp-dir))
			  ,tex-file)
			:output debug-output)
      (uiop:copy-file (merge-pathnames pdf-name tmp-dir) pdf-name))))

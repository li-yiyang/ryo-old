(asdf:defsystem #:ryo
  :author ("凉凉")
  :version "0"
  :description "This is a package where some of my daily lisp code goes in."
  :depends-on ("str" "bt-semaphore" "lparallel")
  :serial t
  :components
  ((:file "package")
   (:file "macro-helper")
   (:file "length")
   (:file "matrix")
   (:file "iter")
   (:file "format")
   (:file "enum")
   (:file "notify")
   (:module statistic
    :pathname "statistic"
    :components
    ((:file "macros")
     (:file "utils")
     (:file "hist-protocol")
     (:file "histogram")
     (:file "histogram-add-info")
     (:file "2d-histogram")
     (:file "2d-histogram-add-info")
     (:file "preview")))))

(defpackage #:ryo
  (:use :cl :ryo.statistics)
  (:export
   ;; length
   #:length=
   #:length<
   #:length>

   ;; matrix
   #:at

   ;; iter
   #:iter-i*
   #:sum-iter-i*
   #:piter-i*
   #:sum-piter-i*
   #:collect-i*
   #:flat-collect-i*
   #:pdolist

   ;; format
   #:errorf
   #:warnf
   #:format-table

   ;; enum
   #:defenum

   ;; notify
   #:notify
   ))

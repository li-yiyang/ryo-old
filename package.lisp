(defpackage #:ryo.statistics
  (:use :cl)
  (:export
   ;; histogram
   #:make-histogram
   #:make-2d-histogram
   #:histogram
   #:2d-histogram
   #:hist-bins
   #:hist-min
   #:hist-max
   #:add-to-hist
   #:hist-rebin!
   #:hist-clear-buffer!
   #:hist-to-csv
   #:hist-to-ascii
   #:ascii-grayscale
   ;; preview
   #:random-samples
   #:ascii-table-form
   ))

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

   ;; ===== statistic =====
   #:make-histogram
   #:make-2d-histogram
   #:histogram
   #:2d-histogram
   #:hist-bins
   #:hist-min
   #:hist-max
   #:add-to-hist
   #:hist-rebin!
   #:hist-clear-buffer!
   #:hist-to-csv
   #:hist-to-ascii
   #:ascii-grayscale

   #:random-samples
   #:ascii-table-form
   ))

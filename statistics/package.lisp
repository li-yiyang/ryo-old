(defpackage #:ryo/statistics
  (:use :cl)
  (:export
   ;; histogram
   #:histogram
   #:2d-histogram
   #:histogram-add-info
   #:2d-histogram-add-info

   #:make-histogram
   #:make-2d-histogram
   #:make-histogram-add-info
   #:make-2d-histogram-add-info

   #:info-fn

   #:hist-bins
   #:hist-min
   #:hist-max

   #:add-to-hist

   #:hist-rebin!
   #:hist-clear-buffer!
   #:hist-clear-all!
   #:hist-mean
   #:hist-vector-euclid-norm
   #:hist-normed-reduce
   #:hist-normed-mean
   #:hist-dump-empty
   #:hist-dump
   #:hist-shape-eq
   #:hist-add

   #:hist-to-csv
   #:hist-to-ascii
   #:hist-iter-over
   #:ascii-grayscale

   ;; preview
   #:random-samples
   #:ascii-table-form
   ))

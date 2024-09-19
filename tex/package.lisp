(defpackage #:ryo/tex
  (:use :cl)
  (:export
   ;; see `utils.lisp'
   #:tex
   #:tex-cmd
   #:mk-tex-name
   #:tex-env
   #:when-str

   #:widget
   #:container
   #:title

   #:plain
   #:image

   #:document
   #:define-document
   #:beamer
   #:article

   #:environment
   #:define-environment
   #:frame

   ;; see `protocol.lisp'
   #:parent
   #:id
   ;; #:link-p
   ;; #:linkable
   ;; #:link-to
   #:root
   #:root-p
   #:render
   #:tex-name
   #:add-to
   #:title-p
   #:author-p
   #:title-of
   ))

(in-package :ryo/tex)

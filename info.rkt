#lang info

;; For documentation-rendering purposes

;; ---------------------------------------------------------
;; Package Info

(define collection "glow")
(define deps '("base"))
(define build-deps '("scribble-lib"))

;; ---------------------------------------------------------
;; Collection Info

(define scribblings '(("scribblings/glow.scrbl" (multi-page))))
(define compile-omit-paths 'all)
(define compile-include-files '("main.rkt"))

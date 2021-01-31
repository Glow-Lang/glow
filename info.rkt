#lang info

;; For documentation-rendering purposes

;; ---------------------------------------------------------
;; Package Info

(define collection "glow")
(define deps '("base" "scribble-lib"))

;; ---------------------------------------------------------
;; Collection Info

(define compile-omit-paths 'all)
(define compile-include-files
  '("main.rkt"
    "scribblings/glow-code.rkt"
    "scribblings/glow-explanation.scrbl"
    "scribblings/glow-how-to.scrbl"
    "scribblings/glow-parse.rkt"
    "scribblings/glow-reference-manual.scrbl"
    "scribblings/glow-surface-grammar.scrbl"
    "scribblings/glow-tutorial.scrbl"
    "scribblings/glow.scrbl"))

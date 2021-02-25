#lang racket/base

(provide Glow blockquote not-supported-yet)

(require scribble/manual
         (only-in scribble/core style background-color-property))

(define (Glow) (emph "Glow"))

(define (blockquote . stuff)
  (apply nested #:style 'inset stuff))

(define (not-supported-yet)
  (nested #:style 'inset
          (yellow (bold "NOTE:"))
          " This feature is not supported yet,"
          " though it is in the near-future plans."))

(define (yellow . stuff)
  (apply elem
         #:style
         (style #f (list (background-color-property "yellow")))
         stuff))


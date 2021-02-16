#lang racket/base

(require scribble/manual)

(provide Glow blockquote)

(define (Glow) (emph "Glow"))

(define (blockquote . stuff)
  (apply nested #:style 'inset stuff))

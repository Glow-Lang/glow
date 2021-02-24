#lang scribble/manual

@(require "glow-code.rkt"
          "glow-docs.rkt"
          (only-in scribble/racket make-variable-id)
          (for-label glow)
          (for-syntax racket/base))

@title[#:style '(toc)]{Glow}

@defglowlang[glow]

@local-table-of-contents[]

@include-section["glow-faq.scrbl"]

@include-section["glow-tutorial.scrbl"]

@include-section["glow-reference-manual.scrbl"]

@include-section["glow-DApp-descriptions.scrbl"]

@include-section["glow-how-to.scrbl"]

@include-section["glow-explanation.scrbl"]

@index-section[]

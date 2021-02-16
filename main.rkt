#lang racket/base

;; Define dummy bindings for documentation-rendering purposes

(define-syntax-rule (defdums x ...)
  (begin (provide x ...) (define x #f) ...))

(defdums
  : dot tuple record list
  let type data
  true false if && \|\|  ; make sure to use `read-accept-bar-quote #f` for `\|\|`
  block switch =>
  == input digest sign
  publish! verify! require! assert! deposit! withdraw!
  @interaction @verifiably @publicly @make-interaction @app-interaction
  Unit Int Nat Bool Bytes Digest Assets Signature
  ! <= < > >= + - * / %
  ~~~ &&& \|\|\| ^^^ << >> ; make sure to use `read-accept-bar-quote #f` for `\|\|\|`
  member
  randomUInt256 isValidSignature
  canReach mustReach)

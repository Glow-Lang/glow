(export at-head? at-prefix-normalize)

(import :std/format
        :mukn/glow/compiler/common
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/syntax-context)

;; At `@` annotations come in two forms:
;;  - head form `(@ name thing)` or `(@ (name args ...) thing)`
;;  - prefix form `(@name thing)` or `(@name (args ...) thing)`

;; at-head? : Stx -> Bool
;; Detects head form
(def (at-head? stx)
  (syntax-case stx (@)
    ((@ . _) #t)
    (_ #f)))

;; at-prefix-normalize : Stx -> Stx
;; Normalizes to prefix form, intended to be used for `@verifiably!` and `@publicly!`
(def (at-prefix-normalize stx)
  (syntax-case stx (@)
    ((@ x s) (identifier? #'x)
     (restx1 stx [(prefix-at #'x) #'s]))
    ((@ (x . args) s) (identifier? #'x)
     (restx1 stx [(prefix-at #'x) #'args #'s]))
    (_ stx)))

;; prefix-at : Id -> Id
;; Intended to be used for `@verifiably!` and `@publicly!`
(def (prefix-at x) (restx1 x (string->symbol (format "@~a" (stx-e x)))))

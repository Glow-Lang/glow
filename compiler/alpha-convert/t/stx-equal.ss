(export stx-deep-source=?/at-normalize)

(import :std/format
        :glow/compiler/common
        (for-template :gerbil/core))

;; head-at? : Stx -> Bool
(def (head-at? stx)
  (syntax-case stx (@)
    ((@ . _) #t)
    (_ #f)))

;; prefix-at : Id -> Id
(def (prefix-at x) (restx1 x (string->symbol (format "@~a" (stx-e x)))))

;; head-at-normalize : Stx -> Stx
(def (head-at-normalize stx)
  (syntax-case stx (@)
    ((@ x s) (identifier? #'x)
     (restx1 stx [(prefix-at #'x) #'s]))
    ((@ (x . args) s) (identifier? #'x)
     (restx1 stx [(prefix-at #'x) #'args #'s]))
    (_ stx)))

;; head-at=?/recur : Stx Stx -> Bool
(def (head-at=?/recur a b rec=?)
  (cond ((and (head-at? a) (head-at? b))
         (stx-sexpr=?/recur a b rec=?))
        (else
         (stx-sexpr=?/recur (head-at-normalize a) (head-at-normalize b) rec=?))))

;; stx-deep-source=?/at-normalize : Stx Stx -> Bool
;; only cares about source locations and structure of lists/containers
;; does not care about leaves such as symbols or literals
;; also allows differences between `(@ keyword s)` and `(@keyword s)`
(def (stx-deep-source=?/at-normalize a b)
  (and (stx-shallow-source=? a b)
       (cond
         ((or (stx-leaf? a) (stx-leaf? b))
          (and (stx-leaf? a) (stx-leaf? b)))
         (else
          (head-at=?/recur a b stx-deep-source=?/at-normalize)))))

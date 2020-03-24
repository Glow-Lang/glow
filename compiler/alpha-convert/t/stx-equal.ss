(export stx-deep-source=?/at-normalize)

(import :glow/compiler/common
        :glow/compiler/alpha-convert/at-prefix-normalize)

;; stx-deep-source=?/at-normalize : Stx Stx -> Bool
;; only cares about source locations and structure of lists/containers
;; does not care about leaves such as symbols or literals
;; also allows differences between `(@ keyword s)` and `(@keyword s)`
(def (stx-deep-source=?/at-normalize a b)
  (and (stx-shallow-source=? a b)
       (cond
         ((or (stx-leaf? a) (stx-leaf? b))
          (and (stx-leaf? a) (stx-leaf? b)))
         ((and (at-head? a) (at-head? b))
          (stx-sexpr=?/recur a b stx-deep-source=?/at-normalize))
         (else
          (stx-sexpr=?/recur (at-prefix-normalize a)
                             (at-prefix-normalize b)
                             stx-deep-source=?/at-normalize)))))

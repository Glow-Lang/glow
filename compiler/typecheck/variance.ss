(export #t)

;; A Variance is a (variance Bool Bool)
(defstruct variance (covariant? contravariant?) transparent: #t)
(def irrelevant    (variance #t #t)) ; bottom, ignored
(def covariant     (variance #t #f))
(def contravariant (variance #f #t))
(def invariant     (variance #f #f)) ; top, constrained
;; variance-irrelevant? : Variance -> Bool
(def (variance-irrelevant? v)
  (and (variance-covariant? v) (variance-contravariant? v)))
;; variance-invariant? : Variance -> Bool
(def (variance-invariant? v)
  (and (not (variance-covariant? v)) (not (variance-contravariant? v))))
;; variance-join : Variance Variance -> Variance
;; irrelevant ⊔ a = a
;; invariant ⊔ a = invariant
(def (variance-join a b)
  (with (((variance ap an) a) ((variance bp bn) b))
    (variance (and ap bp) (and an bn))))
;; variance-compose : Variance Variance -> Variance
;; irrelevant ∘ a = irrelevant
;; covariant ∘ a = a
;; contravariant ∘ (variance p n) = (variance n p)
;; invariant ∘ (variance p n) = (variance (and p n) (and p n))
(def (variance-compose a b)
  (with (((variance ap an) a) ((variance bp bn) b))
    (variance (and (or ap bn) (or an bp))
              (and (or ap bp) (or an bn)))))

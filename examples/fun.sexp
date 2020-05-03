;;; Functional programming

;; SKI combinators
(def identity (λ (x) x))
(def konstant (λ (x) (def kx (λ (y) x)) kx))
(def smelting (λ (x) (def sx (λ (y) (def sxy (λ (z) (def xz (x z)) (xz (y z)))) sxy)) sx))

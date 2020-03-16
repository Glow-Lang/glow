(def pick_one
  (λ (b x y) (if b x y)))

(def select
  (λ (p v d) (if (p v) v d)))

(def self
  (λ (x0) (x0 x0)))

(def id_on_int
  (λ (x1)
    (+ x1 1)
    x1))

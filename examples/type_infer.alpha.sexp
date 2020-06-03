(@module
(def pick_one
  (λ (b x y) (if b x y)))

(def select
  (λ (p v d) (if (@app p v) v d)))

(def self
  (λ (x0) (@app x0 x0)))

(def id_on_int
  (λ (x1)
    (@app + x1 1)
    x1)))

(@module
(@debug-label dlb)
(def pick_one
  (λ (b x y) (@debug-label dlb0) (if b x y)))

(@debug-label dlb1)
(def select
  (λ (p v d) (@debug-label dlb2) (if (@app p v) v d)))

(@debug-label dlb3)
(def self
  (λ (x0) (@debug-label dlb4) (@app x0 x0)))

(@debug-label dlb5)
(def id_on_int
  (λ (x1)
    (@debug-label dlb6)
    (@app + x1 1)
    (@debug-label dlb7)
    x1)))

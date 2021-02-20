(@module
(@debug-label dlb)
(def pick_one ()
  (λ (b x y) () (@debug-label dlb0) (switch b (#t x) (#f y))))

(@debug-label dlb1)
(def select ()
  (λ (p v d) () (@debug-label dlb2) (switch (@app p v) (#t v) (#f d))))

(@debug-label dlb3)
(def self ()
  (λ (x0) () (@debug-label dlb4) (@app x0 x0)))

(@debug-label dlb5)
(def id_on_int ()
  (λ (x1) ()
    (@debug-label dlb6)
    (@app + x1 1)
    (@debug-label dlb7)
    x1)))

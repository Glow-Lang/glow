(@module (def pick_one (λ (b x y) (switch b (#t x) (#f y))))
         (def select (λ (p v d) (switch (@app p v) (#t v) (#f d))))
         (def self (λ (x0) (@app x0 x0)))
         (def id_on_int (λ (x1) (@app + x1 1) x1)))

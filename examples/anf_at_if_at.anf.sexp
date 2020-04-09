(def f (λ (a1) (@app + a1 1)))
(def g (λ (a2) (@app * a2 2)))

(@interaction
  ([A B])
  (def inter
    (λ (b x)
      (@ A
        (def y
          (if b
              (block (def tmp (@app f x)) (@app g tmp))
              (block (def tmp0 (@app g x)) (@app f tmp0)))))
      (@tuple))))


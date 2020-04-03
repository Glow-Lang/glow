(def f (λ (a1) a1))
(def g (λ (a2) a2))

(@interaction
  ([A B])
  (def inter
    (λ (b x)
      (@ A
        (def y
          (if b
              (block (def tmp (@app f x)) (@app g tmp))
              (block (def tmp0 (@app g x)) (@app f tmp)))))
      (@tuple))))


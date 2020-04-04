(def f (λ (a1) a1))
(def g (λ (a2) a2))

(@interaction
  ([A B])
  (def inter
    (λ (b x)
      (@ A
        (def y
          (if b
              (@app g (@app f x))
              (@app f (@app g x)))))
      (@tuple))))


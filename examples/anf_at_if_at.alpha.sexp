(def f (λ (a1) (@app + a1 1)))
(def g (λ (a2) (@app * a2 2)))
(@interaction
  ([A B])
  (def inter
    (λ (b x)
      (require! (@app < 0 x))
      (@ A
        (def y
          (if b
              (@app g (@app f x))
              (@app f (@app g x)))))
      (@tuple))))

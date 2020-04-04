(def f (λ (a1) (+ a1 1)))
(def g (λ (a2) (* a2 2)))

(@ (interaction [A B])
   (def inter
     (λ (b x)
       (@ A
          (def y
            (if b
                (g (f x))
                (f (g x)))))
       (@tuple))))

(@module
(def f (λ (a1) () (return (@app + a1 1))))
(def g (λ (a2) () (return (@app * a2 2))))
(def inter
     (@make-interaction
      ((@list A B))
      (b x)
      ()
      (def tmp (@app < 0 x))
      (require! tmp)
      (participant-checkpoint pc #f A)
      (@ A
         (switch b
                 (#t (def tmp0 (@app f x)) (def y (@app g tmp0)))
                 (#f (def tmp1 (@app g x)) (def y (@app f tmp1)))))
      (return (@tuple))))
(return (@tuple)))

(def f () (λ (a1) () (@app + a1 1)))
(def g () (λ (a2) () (@app * a2 2)))
(def inter
     ()
     (@make-interaction
      ((@list A B))
      (b x)
      ()
      (require! (@app < 0 x))
      (@ A (def y () (switch b (1 (@app g (@app f x))) (0 (@app f (@app g x))))))
      (@tuple)))

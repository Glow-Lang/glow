(@module
(@debug-label dlb)
(def f () (λ (a1) () (@debug-label dlb0) (@app + a1 1)))
(@debug-label dlb1)
(def g () (λ (a2) () (@debug-label dlb2) (@app * a2 2)))
(@debug-label dlb3)
(def inter
     ()
     (@make-interaction
      ((@record (participants (@list A B)) (assets (@list DefaultToken))))
      (b x)
      ()
      (@debug-label dlb4)
      (require! (@app < 0 x))
      (@debug-label dlb5)
      (@ A (def y ()
             (switch b
               (#t (@app g (@app f x)))
               (#f (@app f (@app g x))))))
      (@debug-label dlb6)
      (@tuple))))

(@module
(@debug-label dlb)
(def f (λ (a1) (@debug-label dlb0) (@app + a1 1)))
(@debug-label dlb1)
(def g (λ (a2) (@debug-label dlb2) (@app * a2 2)))
(@debug-label dlb3)
(@interaction
  ((@record (participants (@list A B)) (assets (@list DefaultToken))))
  (def inter
    (λ (b x)
      (@debug-label dlb4)
      (require! (@app < 0 x))
      (@debug-label dlb5)
      (@ A
        (def y
          (if b
              (@app g (@app f x))
              (@app f (@app g x)))))
      (@debug-label dlb6)
      (@tuple)))))

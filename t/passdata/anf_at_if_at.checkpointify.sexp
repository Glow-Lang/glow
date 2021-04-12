(@module (begin end)
         (@label begin)
         (@debug-label dlb)
         (def f
              (λ (a1)
                 (begin0 end0)
                 (@label begin0)
                 (@debug-label dlb0)
                 (return (@app + a1 1))
                 (@label end0)))
         (@debug-label dlb1)
         (def g
              (λ (a2)
                 (begin1 end1)
                 (@label begin1)
                 (@debug-label dlb2)
                 (return (@app * a2 2))
                 (@label end1)))
         (@debug-label dlb3)
         (def inter
              (@make-interaction
               ((@record (participants (@list A B)) (assets (@list DefaultToken))))
               (b x)
               (begin2 end2)
               (@label begin2)
               (@debug-label dlb4)
               (def tmp (@app < 0 x))
               (require! tmp)
               (@debug-label dlb5)
               (@label cp)
               (@ A
                  (switch b
                    (#t
                     (def tmp0 (@app f x))
                     (def y (@app g tmp0)))
                    (#f
                     (def tmp1 (@app g x))
                     (def y (@app f tmp1)))))
               (@debug-label dlb6)
               (return (@tuple))
               (@label end2)))
         (return (@tuple))
         (@label end))

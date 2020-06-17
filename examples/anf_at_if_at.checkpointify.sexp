(@module (begin end)
         (@label begin)
         (def f (λ (a1) () (begin0 end0) (@label begin0) (return (@app + a1 1)) (@label end0)))
         (def g (λ (a2) () (begin1 end1) (@label begin1) (return (@app * a2 2)) (@label end1)))
         (def inter
              (@make-interaction
               ((@list A B))
               (b x)
               ()
               (begin2 end2)
               (@label begin2)
               (def tmp (@app < 0 x))
               (require! tmp)
               (@label cp)
               (@ A
                  (switch b
                          (#t (def tmp0 (@app f x)) (def y (@app g tmp0)))
                          (#f (def tmp1 (@app g x)) (def y (@app f tmp1)))))
               (return (@tuple))
               (@label end2)))
         (return (@tuple))
         (@label end))

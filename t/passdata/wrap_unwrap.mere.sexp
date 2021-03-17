(@module (@debug-label dlb)
         (defdata (wrap 'a) (Wrap 'a))
         (def wrap1 (@record))

         (@debug-label dlb0)
         (def unwrap0
           (λ (w0)
             (@debug-label dlb1)
             (switch w0
               ((@app-ctor Wrap _) (@debug-label dlb2) (@tuple)))))

         (@debug-label dlb3)
         (def unwrap1
           (λ (w1)
             (@debug-label dlb4)
             (switch w1
               ((@app-ctor Wrap (@var-pat v1)) (@debug-label dlb5) v1))))

         (@debug-label dlb6)
         (def unwrap2
           (λ (w2)
             (@debug-label dlb7)
             (switch w2
               ((@app-ctor Wrap (@var-pat v2)) (@debug-label dlb8) (@app + 1 v2))))))

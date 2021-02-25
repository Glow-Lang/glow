(@module (@debug-label dlb)
         (def identity
           (λ (x) (@debug-label dlb0) x))
         (@debug-label dlb1)
         (def konstant
           (λ (x0)
             (@debug-label dlb2)
             (def kx (λ (y) (@debug-label dlb3) x0))
             (@debug-label dlb4)
             kx))
         (@debug-label dlb5)
         (def smelting
              (λ (x1)
                 (@debug-label dlb6)
                 (def sx
                      (λ (y0)
                         (@debug-label dlb7)
                         (def sxy
                           (λ (z)
                             (@debug-label dlb8)
                             (def xz (@app x1 z))
                             (@debug-label dlb9)
                             (@app xz (@app y0 z))))
                         (@debug-label dlb10)
                         sxy))
                 (@debug-label dlb11)
                 sx)))

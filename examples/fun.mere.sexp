(@module (def identity (λ (x) x))
         (def konstant (λ (x0) (def kx (λ (y) x0)) kx))
         (def smelting
              (λ (x1)
                 (def sx
                      (λ (y0)
                         (def sxy (λ (z) (def xz (@app x1 z)) (@app xz (@app y0 z))))
                         sxy))
                 sx)))

(@module
(def identity (λ (x) (return x)))
(def konstant (λ (x0) (def kx (λ (y) (return x0))) (return kx)))
(def smelting
     (λ (x1)
        (def sx
             (λ (y0)
                (def sxy (λ (z) (def xz (@app x1 z)) (def tmp (@app y0 z)) (return (@app xz tmp))))
                (return sxy)))
        (return sx)))
(return (@tuple)))

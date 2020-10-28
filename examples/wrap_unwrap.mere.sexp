(@module (defdata (wrap 'a) (Wrap 'a))
         (def wrap1
              (@record (input (λ (tag)
                                 (def x (input (wrap 'a) tag))
                                 x))))
         (def unwrap0 (λ (w0) (switch w0 ((@app-ctor Wrap _) (@tuple)))))
         (def unwrap1 (λ (w1) (switch w1 ((@app-ctor Wrap (@var-pat v1)) v1))))
         (def unwrap2 (λ (w2) (switch w2 ((@app-ctor Wrap (@var-pat v2)) (@app + 1 v2))))))

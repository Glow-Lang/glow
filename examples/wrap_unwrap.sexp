;; 'a is invariant for now, though it could be
;; inferred as covariant
(defdata (wrap 'a) (Wrap 'a))
#|
(def unwrap0
  (λ (w0)
    (switch w0
      ((Wrap _) (@tuple)))))
|#
(def unwrap1
  (λ (w1)
    (switch w1
      ((Wrap v1) v1))))
#|
(def unwrap2
  (λ (w2)
    (switch w2
      ((Wrap v2) (+ 1 v2)))))
|#

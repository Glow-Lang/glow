;; 'a is invariant for now, though it could be
;; inferred as covariant
(defdata (wrap 'a) (Wrap 'a))

(def unwrap0
  (λ (w0)
    (switch w0
      ((Wrap _) (@tuple)))))

(def unwrap1
  (λ (w1)
    (switch w1
      ((Wrap v1) v1))))

(def unwrap2
  (λ (w2)
    (switch w2
      ((Wrap v2) (+ 1 v2)))))

#|
;; Run once arrow types are supported in the surface syntax
;; 'x is invariant because consume and produce
(defdata (consume_produce 'x)
  (ConsumeProduce (-> 'x (@tuple))
                  (-> (@tuple) 'x)))

(def consume0
  (λ (cp0 x0)
    (switch cp0
      ((ConsumeProduce c0 _) (c x0)))))

(def produce1
  (λ (cp1)
    (switch cp1
      ((ConsumeProduce _ p1) (p1 (@tuple))))))

(def produce_consume2
  (λ (cp2)
    (switch cp2
      ((ConsumeProduce c2 p2) (c2 (p2 (@tuple)))))))

(def consume_produce3
  (λ (cp3 x3)
    (switch cp3
      ((ConsumeProduce c3 p3)
       (c3 x3)
       (p3 (@tuple))))))
|#

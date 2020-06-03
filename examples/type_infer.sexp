(@module
;; Examples taken from MLsub:
;;  - Video: ICFP 2015 03 Polymorphism, subtyping, and type inference in MLsub
;;    https://www.youtube.com/watch?v=E3PIKlsXOQo
;;  - Video: POPL 2017 Polymorphism, subtyping and type inference in MLsub -- Stephen Dolan - Alan Mycroft
;;    https://www.youtube.com/watch?v=-P1ks4NPIyk
;;  - Slides: Polymorphism, Subtyping, andType Inference in MLsub
;;    https://www.cl.cam.ac.uk/~sd601/lfcs_slides.pdf
;;    http://stedolan.net/research/mlsub-slides-long.pdf
;;  - Paper: Polymorphism, Subtyping, and Type Inference in MLsub
;;    https://www.cl.cam.ac.uk/%7Esd601/papers/mlsub-preprint.pdf
;;    https://dl.acm.org/doi/10.1145/3093333.3009882
;;  - Demo: MLsub type inference
;;    https://www.cl.cam.ac.uk/~sd601/mlsub/
;;    https://github.com/stedolan/mlsub
;;  - Thesis: Stephen Dolan Algebraic Subtyping
;;    https://www.cl.cam.ac.uk/~sd601/thesis.pdf

(def pick_one ; (-> bool 'a 'a 'a)
  (λ (b x y) (if b x y)))

(def select ; (-> (-> 'a bool) 'a 'b (∪ 'a 'b))
  (λ (p v d) (if (p v) v d)))

#|
(defdata (conslist 'a)
  Empty
  (Cons 'a (conslist 'a)))

(defrec map
  (λ (f xs)
    (switch xs
      (Empty Empty)
      ((Cons x ys) (Cons (f x) (map f ys))))))
|#

(def self ; (-> (∩ (-> 'a 'b) 'a) 'b)
  (λ (x) (x x)))

(def id_on_int ; (-> (∩ int 'a) 'a)
  (λ (x)
    (+ x 1)
    x)))

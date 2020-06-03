(@module
(def i 2)
(switch i
  (0 "zero")
  (1 "one")
  (2 "two")
  (_ (if (< 0 i) "many" "negative")))

(def b #f)
(switch b
  (#t "yes")
  (#f "no"))

(def bs "apple")
(switch bs
  ("apple" "orange")
  ("banana" "forage")
  (_ "storage"))

(def l (@list 1 2))
(switch l
  ((@list 0 1) "dore")
  ((@list 1 2) "remi")
  ((@list 3 6 0) "fatido")
  (_ "other"))

(def p (@tuple 1 2))
(switch p ((@tuple a b) (+ a b)))

(def v (@record (x 3) (y 4)))
(switch v
  ((@record (x x) (y y)) (sqrt (+ (sqr x) (sqr y)))))

(defdata lcexpr
  (Var int)
  (Lam lcexpr)
  (App lcexpr lcexpr))
(def omega (App (Lam (App (Var 0) (Var 0)))
                (Lam (App (Var 0) (Var 0)))))
(def freevars
  (λ ((e : lcexpr))
    (switch e
      ((Var x) (@list x))
      (_ (@list 0 1 2 3 4 5 6 7 8 9))))) ; TODO: fix when we add recursion
(switch omega
  ((App (Lam b) a) "beta")
  ((Lam (App f (Var 0)))
   (if (not (member 0 (freevars f)))
       "eta"
       "not immediate"))
  (_ "not immediate"))

(defdata ymn Yes Maybe No)
(def ans Maybe)
(def possible
  (λ ((a : ymn)) : bool
    (switch a
      ((@or-pat Yes Maybe) #t)
      (No #f))))
(def definite
  (λ ((a : ymn)) : bool
    (switch a
      (Yes #t)
      ((@or-pat No Maybe) #f))))
(switch (@tuple (possible ans) (definite ans))
  ((@tuple #t #t) "yes")
  ((@tuple #t #f) "maybe")
  ((@tuple #f #f) "no")
  ((@tuple #f #t) "inconceivable!")))

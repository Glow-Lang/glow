(@module
(def i 2)
(switch i
  (0 "zero")
  (1 "one")
  (2 "two")
  (_ (if (@app < 0 i) "many" "negative")))

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
(switch p ((@tuple (@var-pat a) (@var-pat b0)) (@app + a b0)))

(def v (@record (x 3) (y 4)))
(switch v
  ((@record (x (@var-pat x)) (y (@var-pat y)))
   (@app + (@app * x x) (@app * y y))))

(defdata lcexpr
  (Var Int)
  (Lam lcexpr)
  (App lcexpr lcexpr))
(def omega
  (@app App (@app Lam (@app App (@app Var 0) (@app Var 0)))
            (@app Lam (@app App (@app Var 0) (@app Var 0)))))
(def freevars
  (λ ((e : lcexpr))
    (switch e
      ((@app-ctor Var (@var-pat x0)) (@list x0))
      (_ (@list 0 1 2 3 4 5 6 7 8 9))))) ; TODO: fix when we add recursion
(switch omega
  ((@app-ctor App (@app-ctor Lam (@var-pat b1)) (@var-pat a0)) "beta")
  ((@app-ctor Lam (@app-ctor App (@var-pat f) (@app-ctor Var 0)))
   (if (@app not (@app member 0 (@app freevars f)))
       "eta"
       "not immediate"))
  (_ "not immediate"))

(defdata ymn Yes Maybe No)
(def ans Maybe)
(def possible
  (λ ((a1 : ymn)) : Bool
    (switch a1
      ((@or-pat (@app-ctor Yes) (@app-ctor Maybe)) #t)
      ((@app-ctor No) #f))))
(def definite
  (λ ((a2 : ymn)) : Bool
    (switch a2
      ((@app-ctor Yes) #t)
      ((@or-pat (@app-ctor No) (@app-ctor Maybe)) #f))))
(switch (@tuple (@app possible ans) (@app definite ans))
  ((@tuple #t #t) "yes")
  ((@tuple #t #f) "maybe")
  ((@tuple #f #f) "no")
  ((@tuple #f #t) "inconceivable!")))

(def i () 2)
(switch i (0 "zero") (1 "one") (2 "two") (_ (switch (@app < 0 i) (#t "many") (#f "negative"))))
(def b () #f)
(switch b (#t "yes") (#f "no"))
(def bs () "apple")
(switch bs ("apple" "orange") ("banana" "forage") (_ "storage"))
(def l () (@list 1 2))
(switch l ((@list 0 1) "dore") ((@list 1 2) "remi") ((@list 3 6 0) "fatido") (_ "other"))
(def p () (@tuple 1 2))
(switch p ((@tuple a b0) (@app + a b0)))
(def v () (@record (x 3) (y 4)))
(switch v ((@record (x x) (y y)) (@app sqrt (@app + (@app sqr x) (@app sqr y)))))
(defdata lcexpr
         (Var int)
         (Lam lcexpr)
         (App lcexpr lcexpr)
         with:
         (@record (input (λ (tag) (: lcexpr) (def x1 (: lcexpr) (input lcexpr tag)) x1))))
(def omega
     ()
     (@app App
           (@app Lam (@app App (@app Var 0) (@app Var 0)))
           (@app Lam (@app App (@app Var 0) (@app Var 0)))))
(def freevars
     ()
     (λ ((e : lcexpr)) () (switch e ((Var x0) (@list x0)) (_ (@list 0 1 2 3 4 5 6 7 8 9)))))
(switch omega
        ((App (Lam b1) a0) "beta")
        ((Lam (App f (Var 0)))
         (switch (@app not (@app member 0 (@app freevars f))) (#t "eta") (#f "not immediate")))
        (_ "not immediate"))
(defdata ymn
         Yes
         Maybe
         No
         with:
         (@record (input (λ (tag0) (: ymn) (def x2 (: ymn) (input ymn tag0)) x2))
                  (toNat (λ ((x3 : ymn)) (: nat) (switch x3 (Yes 0) (Maybe 1) (No 2))))
                  (ofNat (λ ((x4 : nat)) (: ymn) (switch x4 (0 Yes) (1 Maybe) (2 No))))))
(def ans () Maybe)
(def possible () (λ ((a1 : ymn)) (: bool) (switch a1 ((@or-pat Yes Maybe) #t) (No #f))))
(def definite () (λ ((a2 : ymn)) (: bool) (switch a2 (Yes #t) ((@or-pat No Maybe) #f))))
(switch (@tuple (@app possible ans) (@app definite ans))
        ((@tuple #t #t) "yes")
        ((@tuple #t #f) "maybe")
        ((@tuple #f #f) "no")
        ((@tuple #f #t) "inconceivable!"))

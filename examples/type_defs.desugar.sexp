(@module
(@debug-label dlb)
(defdata yn
         Yes
         No
         with:
         (@record (input (λ (tag) (: yn) (def x (: yn) (input yn tag)) x))
                  (toNat (λ ((x0 : yn)) (: Nat) (switch x0 ((@app-ctor Yes) 0) ((@app-ctor No) 1))))
                  (ofNat (λ ((x1 : Nat)) (: yn) (switch x1 (0 Yes) (1 No))))))

(@debug-label dlb0)
(defdata ordering
         LT
         EQ
         GT
         with:
         (@record (input (λ (tag0) (: ordering) (def x2 (: ordering) (input ordering tag0)) x2))
                  (toNat (λ ((x3 : ordering)) (: Nat) (switch x3 ((@app-ctor LT) 0) ((@app-ctor EQ) 1) ((@app-ctor GT) 2))))
                  (ofNat (λ ((x4 : Nat)) (: ordering) (switch x4 (0 LT) (1 EQ) (2 GT))))))

(@debug-label dlb1)
(defdata pos2d
         (Posn Int Int)
         with:
         (@record (input (λ (tag1) (: pos2d) (def x5 (: pos2d) (input pos2d tag1)) x5))))

(@debug-label dlb2)
(deftype colorRGB (@record (r Int) (g Int) (b Int)))

(@debug-label dlb3)
(defdata (pair 'a 'b)
         (Pair 'a 'b)
         with:
         (@record))

(@debug-label dlb4)
(def pair_tuple ()
  (λ ((p : (pair 'a 'b))) (: (@tuple 'a 'b))
    (@debug-label dlb5)
    (switch p
      ((@app-ctor Pair (@var-pat a) (@var-pat b))
       (@debug-label dlb6)
       (@tuple a b)))))

(@debug-label dlb7)
(def tuple_pair
     ()
     (λ ((t : (@tuple 'a 'b))) (: (pair 'a 'b))
       (@debug-label dlb8)
       (switch t
         ((@tuple (@var-pat a0) (@var-pat b0))
          (@debug-label dlb9)
          (@app Pair a0 b0)))))

(@debug-label dlb10)
(defdata (option 'a)
         (Some 'a)
         None
         with:
         (@record))

(@debug-label dlb11)
(defdata (result 'a 'b)
         (Ok 'a)
         (Error 'b)
         with:
         (@record))

(@debug-label dlb12)
(def option_result
     ()
     (λ ((o : (option 'a)))
        (: (result 'a (@tuple)))
        (@debug-label dlb13)
        (switch o
          ((@app-ctor Some (@var-pat a1))
           (@debug-label dlb14)
           (@app Ok a1))
          ((@app-ctor None)
           (@debug-label dlb15)
           (@app Error (@tuple))))))

(@debug-label dlb16)
(defdata natural
         Zero
         (Succ natural)
         with:
         (@record (input (λ (tag2) (: natural) (def x6 (: natural) (input natural tag2)) x6))))

(@debug-label dlb17)
(defdata (conslist 'a)
         Empty
         (Cons 'a (conslist 'a))
         with:
         (@record))

(@debug-label dlb18)
(deftype (assocpairlist 'a 'b) (conslist (pair 'a 'b)))

(@debug-label dlb19)
(defdata (lcexpr 'lit)
         (Lit 'lit)
         (Var natural)
         (Lam (lcexpr 'lit))
         (App (lcexpr 'lit) (lcexpr 'lit))
         with:
         (@record))

(@debug-label dlb20)
(deftype lcintexpr (lcexpr Int))

(@debug-label dlb21)
(defdata nothing
         with:
         (@record (input (λ (tag3) (: nothing) (def x7 (: nothing) (input nothing tag3)) x7))
                  (toNat (λ ((x8 : nothing)) (: Nat) (switch x8)))
                  (ofNat (λ ((x9 : Nat)) (: nothing) (switch x9)))))

(@debug-label dlb22)
(deftype purelcexpr (lcexpr nothing)))

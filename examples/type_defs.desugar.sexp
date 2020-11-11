(@module
(defdata yn
         Yes
         No
         with:
         (@record (input (λ (tag) (: yn) (def x (: yn) (input yn tag)) x))
                  (toNat (λ ((x0 : yn)) (: nat) (switch x0 ((@app-ctor Yes) 0) ((@app-ctor No) 1))))
                  (ofNat (λ ((x1 : nat)) (: yn) (switch x1 (0 Yes) (1 No))))))
(defdata ordering
         LT
         EQ
         GT
         with:
         (@record (input (λ (tag0) (: ordering) (def x2 (: ordering) (input ordering tag0)) x2))
                  (toNat (λ ((x3 : ordering)) (: nat) (switch x3 ((@app-ctor LT) 0) ((@app-ctor EQ) 1) ((@app-ctor GT) 2))))
                  (ofNat (λ ((x4 : nat)) (: ordering) (switch x4 (0 LT) (1 EQ) (2 GT))))))
(defdata pos2d
         (Posn int int)
         with:
         (@record (input (λ (tag1) (: pos2d) (def x5 (: pos2d) (input pos2d tag1)) x5))))
(deftype colorRGB (@record (r int) (g int) (b int)))
(defdata (pair 'a 'b)
         (Pair 'a 'b)
         with:
         (@record))
(def pair_tuple () (λ ((p : (pair 'a 'b))) (: (@tuple 'a 'b)) (switch p ((@app-ctor Pair (@var-pat a) (@var-pat b)) (@tuple a b)))))
(def tuple_pair
     ()
     (λ ((t : (@tuple 'a 'b))) (: (pair 'a 'b)) (switch t ((@tuple (@var-pat a0) (@var-pat b0)) (@app Pair a0 b0)))))
(defdata (option 'a)
         (Some 'a)
         None
         with:
         (@record))
(defdata (result 'a 'b)
         (Ok 'a)
         (Error 'b)
         with:
         (@record))
(def option_result
     ()
     (λ ((o : (option 'a)))
        (: (result 'a (@tuple)))
        (switch o ((@app-ctor Some (@var-pat a1)) (@app Ok a1)) ((@app-ctor None) (@app Error (@tuple))))))
(defdata natural
         Zero
         (Succ natural)
         with:
         (@record (input (λ (tag2) (: natural) (def x6 (: natural) (input natural tag2)) x6))))
(defdata (conslist 'a)
         Empty
         (Cons 'a (conslist 'a))
         with:
         (@record))
(deftype (assocpairlist 'a 'b) (conslist (pair 'a 'b)))
(defdata (lcexpr 'lit)
         (Lit 'lit)
         (Var natural)
         (Lam (lcexpr 'lit))
         (App (lcexpr 'lit) (lcexpr 'lit))
         with:
         (@record))
(deftype lcintexpr (lcexpr int))
(defdata nothing
         with:
         (@record (input (λ (tag3) (: nothing) (def x7 (: nothing) (input nothing tag3)) x7))
                  (toNat (λ ((x8 : nothing)) (: nat) (switch x8)))
                  (ofNat (λ ((x9 : nat)) (: nothing) (switch x9)))))
(deftype purelcexpr (lcexpr nothing)))

(@module
(defdata yn
         Yes
         No
         with:
         (@record (input (λ (tag) (: yn) (def x (: yn) (input yn tag)) x))
                  (toNat (λ ((x0 : yn)) (: nat) (switch x0 (Yes 0) (No 1))))
                  (ofNat (λ ((x1 : nat)) (: yn) (switch x1 (0 Yes) (1 No))))))
(defdata ordering
         LT
         EQ
         GT
         with:
         (@record (input (λ (tag0) (: ordering) (def x2 (: ordering) (input ordering tag0)) x2))
                  (toNat (λ ((x3 : ordering)) (: nat) (switch x3 (LT 0) (EQ 1) (GT 2))))
                  (ofNat (λ ((x4 : nat)) (: ordering) (switch x4 (0 LT) (1 EQ) (2 GT))))))
(defdata pos2d
         (Posn int int)
         with:
         (@record (input (λ (tag1) (: pos2d) (def x5 (: pos2d) (input pos2d tag1)) x5))))
(deftype colorRGB (@record (r int) (g int) (b int)))
(defdata (pair 'a 'b)
         (Pair 'a 'b)
         with:
         (@record (input (λ (tag2)
                            (: (pair 'a 'b))
                            (def x6 (: (pair 'a 'b)) (input (pair 'a 'b) tag2))
                            x6))))
(def pair_tuple () (λ ((p : (pair 'a 'b))) (: (@tuple 'a 'b)) (switch p ((Pair a b) (@tuple a b)))))
(def tuple_pair
     ()
     (λ ((t : (@tuple 'a 'b))) (: (pair 'a 'b)) (switch t ((@tuple a0 b0) (@app Pair a0 b0)))))
(defdata (option 'a)
         (Some 'a)
         None
         with:
         (@record (input (λ (tag3)
                            (: (option 'a))
                            (def x7 (: (option 'a)) (input (option 'a) tag3))
                            x7))))
(defdata (result 'a 'b)
         (Ok 'a)
         (Error 'b)
         with:
         (@record (input (λ (tag4)
                            (: (result 'a 'b))
                            (def x8 (: (result 'a 'b)) (input (result 'a 'b) tag4))
                            x8))))
(def option_result
     ()
     (λ ((o : (option 'a)))
        (: (result 'a (@tuple)))
        (switch o ((Some a1) (@app Ok a1)) (None (@app Error (@tuple))))))
(defdata natural
         Zero
         (Succ natural)
         with:
         (@record (input (λ (tag5) (: natural) (def x9 (: natural) (input natural tag5)) x9))))
(defdata (conslist 'a)
         Empty
         (Cons 'a (conslist 'a))
         with:
         (@record (input (λ (tag6)
                            (: (conslist 'a))
                            (def x10 (: (conslist 'a)) (input (conslist 'a) tag6))
                            x10))))
(deftype (assocpairlist 'a 'b) (conslist (pair 'a 'b)))
(defdata (lcexpr 'lit)
         (Lit 'lit)
         (Var natural)
         (Lam (lcexpr 'lit))
         (App (lcexpr 'lit) (lcexpr 'lit))
         with:
         (@record (input (λ (tag7)
                            (: (lcexpr 'lit))
                            (def x11 (: (lcexpr 'lit)) (input (lcexpr 'lit) tag7))
                            x11))))
(deftype lcintexpr (lcexpr int))
(defdata nothing
         with:
         (@record (input (λ (tag8) (: nothing) (def x12 (: nothing) (input nothing tag8)) x12))
                  (toNat (λ ((x13 : nothing)) (: nat) (switch x13)))
                  (ofNat (λ ((x14 : nat)) (: nothing) (switch x14)))))
(deftype purelcexpr (lcexpr nothing)))

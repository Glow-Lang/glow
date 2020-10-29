(@module (defdata yn Yes No)
         (def yn1
              (@record (input (λ (tag) (def x (input yn tag)) x))
                       (toNat (λ (x0) (switch x0 ((@app-ctor Yes) 0) ((@app-ctor No) 1))))
                       (ofNat (λ (x1) (switch x1 (0 Yes) (1 No))))))
         (defdata ordering LT EQ GT)
         (def ordering1
              (@record (input (λ (tag0)
                                 (def x2 (input ordering tag0))
                                 x2))
                       (toNat (λ (x3) (switch x3 ((@app-ctor LT) 0) ((@app-ctor EQ) 1) ((@app-ctor GT) 2))))
                       (ofNat (λ (x4) (switch x4 (0 LT) (1 EQ) (2 GT))))))
         (defdata pos2d (Posn int int))
         (def pos2d1
              (@record (input (λ (tag1) (def x5 (input pos2d tag1)) x5))))
         (deftype colorRGB (@record (r int) (g int) (b int)))
         (defdata (pair 'a 'b) (Pair 'a 'b))
         (def pair1
              (@record (input (λ (tag2)
                                 (def x6 (input (pair 'a 'b) tag2))
                                 x6))))
         (def pair_tuple
              (λ (p) (switch p ((@app-ctor Pair (@var-pat a) (@var-pat b)) (@tuple a b)))))
         (def tuple_pair
              (λ (t)
                 (switch t ((@tuple (@var-pat a0) (@var-pat b0)) (@app Pair a0 b0)))))
         (defdata (option 'a) (Some 'a) None)
         (def option1
              (@record (input (λ (tag3)
                                 (def x7 (input (option 'a) tag3))
                                 x7))))
         (defdata (result 'a 'b) (Ok 'a) (Error 'b))
         (def result1
              (@record (input (λ (tag4)
                                 (def x8 (input (result 'a 'b) tag4))
                                 x8))))
         (def option_result
              (λ (o)
                 (switch o ((@app-ctor Some (@var-pat a1)) (@app Ok a1)) ((@app-ctor None) (@app Error (@tuple))))))
         (defdata natural Zero (Succ natural))
         (def natural1
              (@record (input (λ (tag5)
                                 (def x9 (input natural tag5))
                                 x9))))
         (defdata (conslist 'a) Empty (Cons 'a (conslist 'a)))
         (def conslist1
              (@record (input (λ (tag6)
                                 (def x10 (input (conslist 'a) tag6))
                                 x10))))
         (deftype (assocpairlist 'a 'b) (conslist (pair 'a 'b)))
         (defdata (lcexpr 'lit)
                  (Lit 'lit)
                  (Var natural)
                  (Lam (lcexpr 'lit))
                  (App (lcexpr 'lit) (lcexpr 'lit)))
         (def lcexpr1
              (@record (input (λ (tag7)
                                 (def x11 (input (lcexpr 'lit) tag7))
                                 x11))))
         (deftype lcintexpr (lcexpr int))
         (defdata nothing)
         (def nothing1
              (@record (input (λ (tag8)
                                 (def x12 (input nothing tag8))
                                 x12))
                       (toNat (λ (x13) (switch x13)))
                       (ofNat (λ (x14) (switch x14)))))
         (deftype purelcexpr (lcexpr nothing)))

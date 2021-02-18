(@module (@debug-label dlb)
         (defdata yn Yes No)
         (def yn1
              (@record (input (λ (tag) (def x (input yn tag)) x))
                       (toNat (λ (x0) (switch x0 ((@app-ctor Yes) 0) ((@app-ctor No) 1))))
                       (ofNat (λ (x1) (switch x1 (0 Yes) (1 No))))))

         (@debug-label dlb0)
         (defdata ordering LT EQ GT)
         (def ordering1
              (@record (input (λ (tag0)
                                 (def x2 (input ordering tag0))
                                 x2))
                       (toNat (λ (x3) (switch x3 ((@app-ctor LT) 0) ((@app-ctor EQ) 1) ((@app-ctor GT) 2))))
                       (ofNat (λ (x4) (switch x4 (0 LT) (1 EQ) (2 GT))))))

         (@debug-label dlb1)
         (defdata pos2d (Posn Int Int))
         (def pos2d1
              (@record (input (λ (tag1) (def x5 (input pos2d tag1)) x5))))

         (@debug-label dlb2)
         (deftype colorRGB (@record (r Int) (g Int) (b Int)))

         (@debug-label dlb3)
         (defdata (pair 'a 'b) (Pair 'a 'b))
         (def pair1 (@record))

         (@debug-label dlb4)
         (def pair_tuple
              (λ (p)
                (@debug-label dlb5)
                (switch p
                  ((@app-ctor Pair (@var-pat a) (@var-pat b))
                   (@debug-label dlb6)
                   (@tuple a b)))))

         (@debug-label dlb7)
         (def tuple_pair
              (λ (t)
                 (@debug-label dlb8)
                 (switch t
                   ((@tuple (@var-pat a0) (@var-pat b0))
                    (@debug-label dlb9)
                    (@app Pair a0 b0)))))

         (@debug-label dlb10)
         (defdata (option 'a) (Some 'a) None)
         (def option1 (@record))

         (@debug-label dlb11)
         (defdata (result 'a 'b) (Ok 'a) (Error 'b))
         (def result1 (@record))

         (@debug-label dlb12)
         (def option_result
              (λ (o)
                 (@debug-label dlb13)
                 (switch o
                   ((@app-ctor Some (@var-pat a1))
                    (@debug-label dlb14)
                    (@app Ok a1))
                   ((@app-ctor None)
                    (@debug-label dlb15)
                    (@app Error (@tuple))))))

         (@debug-label dlb16)
         (defdata natural Zero (Succ natural))
         (def natural1
              (@record (input (λ (tag2)
                                 (def x6 (input natural tag2))
                                 x6))))

         (@debug-label dlb17)
         (defdata (conslist 'a) Empty (Cons 'a (conslist 'a)))
         (def conslist1 (@record))

         (@debug-label dlb18)
         (deftype (assocpairlist 'a 'b) (conslist (pair 'a 'b)))

         (@debug-label dlb19)
         (defdata (lcexpr 'lit)
                  (Lit 'lit)
                  (Var natural)
                  (Lam (lcexpr 'lit))
                  (App (lcexpr 'lit) (lcexpr 'lit)))
         (def lcexpr1 (@record))

         (@debug-label dlb20)
         (deftype lcintexpr (lcexpr Int))

         (@debug-label dlb21)
         (defdata nothing)
         (def nothing1
              (@record (input (λ (tag3)
                                 (def x7 (input nothing tag3))
                                 x7))
                       (toNat (λ (x8) (switch x8)))
                       (ofNat (λ (x9) (switch x9)))))

         (@debug-label dlb22)
         (deftype purelcexpr (lcexpr nothing)))

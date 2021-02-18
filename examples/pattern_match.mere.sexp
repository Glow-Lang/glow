(@module (@debug-label dlb)
         (def i 2)
         (@debug-label dlb0)
         (switch i
                 (0 (@debug-label dlb1) "zero")
                 (1 (@debug-label dlb2) "one")
                 (2 (@debug-label dlb3) "two")
                 (_ (@debug-label dlb4) (switch (@app < 0 i) (#t "many") (#f "negative"))))

         (@debug-label dlb5)
         (def b #f)
         (@debug-label dlb6)
         (switch b
           (#t (@debug-label dlb7) "yes")
           (#f (@debug-label dlb8) "no"))

         (@debug-label dlb9)
         (def bs "apple")
         (@debug-label dlb10)
         (switch bs
           ("apple" (@debug-label dlb11) "orange")
           ("banana" (@debug-label dlb12) "forage")
           (_ (@debug-label dlb13) "storage"))

         (@debug-label dlb14)
         (def l (@list 1 2))
         (@debug-label dlb15)
         (switch l
           ((@list 0 1) (@debug-label dlb16) "dore")
           ((@list 1 2) (@debug-label dlb17) "remi")
           ((@list 3 6 0) (@debug-label dlb18) "fatido")
           (_ (@debug-label dlb19) "other"))

         (@debug-label dlb20)
         (def p (@tuple 1 2))
         (@debug-label dlb21)
         (switch p
           ((@tuple (@var-pat a) (@var-pat b0))
            (@debug-label dlb22)
            (@app + a b0)))

         (@debug-label dlb23)
         (def v (@record (x 3) (y 4)))
         (@debug-label dlb24)
         (switch v
           ((@record (x (@var-pat x)) (y (@var-pat y)))
            (@debug-label dlb25)
            (@app + (@app * x x) (@app * y y))))

         (@debug-label dlb26)
         (defdata lcexpr (Var Int) (Lam lcexpr) (App lcexpr lcexpr))
         (def lcexpr1
              (@record (input (λ (tag) (def x1 (input lcexpr tag)) x1))))
         (@debug-label dlb27)
         (def omega
              (@app App
                    (@app Lam (@app App (@app Var 0) (@app Var 0)))
                    (@app Lam (@app App (@app Var 0) (@app Var 0)))))
         (@debug-label dlb28)
         (def freevars
              (λ (e)
                 (@debug-label dlb29)
                 (switch e
                   ((@app-ctor Var (@var-pat x0))
                    (@debug-label dlb30)
                    (@list x0))
                   (_
                    (@debug-label dlb31)
                    (@list 0 1 2 3 4 5 6 7 8 9)))))
         (@debug-label dlb32)
         (switch omega
                 ((@app-ctor App (@app-ctor Lam (@var-pat b1)) (@var-pat a0))
                  (@debug-label dlb33)
                  "beta")
                 ((@app-ctor Lam (@app-ctor App (@var-pat f) (@app-ctor Var 0)))
                  (@debug-label dlb34)
                  (switch (@app not (@app member 0 (@app freevars f)))
                          (#t "eta")
                          (#f "not immediate")))
                 (_
                  (@debug-label dlb35)
                  "not immediate"))

         (@debug-label dlb36)
         (defdata ymn Yes Maybe No)
         (def ymn1
              (@record (input (λ (tag0) (def x2 (input ymn tag0)) x2))
                       (toNat (λ (x3) (switch x3 ((@app-ctor Yes) 0) ((@app-ctor Maybe) 1) ((@app-ctor No) 2))))
                       (ofNat (λ (x4) (switch x4 (0 Yes) (1 Maybe) (2 No))))))
         (@debug-label dlb37)
         (def ans Maybe)
         (@debug-label dlb38)
         (def possible
           (λ (a1)
             (@debug-label dlb39)
             (switch a1
               ((@or-pat (@app-ctor Yes) (@app-ctor Maybe))
                (@debug-label dlb40)
                #t)
               ((@app-ctor No)
                (@debug-label dlb41)
                #f))))
         (@debug-label dlb42)
         (def definite
           (λ (a2)
             (@debug-label dlb43)
             (switch a2
               ((@app-ctor Yes)
                (@debug-label dlb44)
                #t)
               ((@or-pat (@app-ctor No) (@app-ctor Maybe))
                (@debug-label dlb45)
                #f))))
         (@debug-label dlb46)
         (switch (@tuple (@app possible ans) (@app definite ans))
                 ((@tuple #t #t) (@debug-label dlb47) "yes")
                 ((@tuple #t #f) (@debug-label dlb48) "maybe")
                 ((@tuple #f #f) (@debug-label dlb49) "no")
                 ((@tuple #f #t) (@debug-label dlb50) "inconceivable!")))

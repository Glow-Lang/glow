(@module
(@debug-label dlb)
(defdata yn Yes No)

(@debug-label dlb0)
(defdata ordering LT EQ GT)

(@debug-label dlb1)
(defdata pos2d (Posn Int Int))

(@debug-label dlb2)
(deftype colorRGB (@record (r Int) (g Int) (b Int)))

(@debug-label dlb3)
(defdata (pair 'a 'b) (Pair 'a 'b))

(@debug-label dlb4)
(def pair_tuple
  (λ ((p : (pair 'a 'b))) : (@tuple 'a 'b)
    (@debug-label dlb5)
    (switch p
      ((@app-ctor Pair (@var-pat a) (@var-pat b))
       (@debug-label dlb6)
       (@tuple a b)))))

(@debug-label dlb7)
(def tuple_pair
  (λ ((t : (@tuple 'a 'b))) : (pair 'a 'b)
    (@debug-label dlb8)
    (switch t
      ((@tuple (@var-pat a0) (@var-pat b0))
       (@debug-label dlb9)
       (@app Pair a0 b0)))))

(@debug-label dlb10)
(defdata (option 'a) (Some 'a) None)

(@debug-label dlb11)
(defdata (result 'a 'b) (Ok 'a) (Error 'b))

(@debug-label dlb12)
(def option_result
  (λ ((o : (option 'a))) : (result 'a (@tuple))
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
  (Succ natural))

(@debug-label dlb17)
(defdata (conslist 'a)
  Empty
  (Cons 'a (conslist 'a)))

(@debug-label dlb18)
(deftype (assocpairlist 'a 'b) (conslist (pair 'a 'b)))

(@debug-label dlb19)
(defdata (lcexpr 'lit)
  (Lit 'lit)
  (Var natural)
  (Lam (lcexpr 'lit))
  (App (lcexpr 'lit) (lcexpr 'lit)))

(@debug-label dlb20)
(deftype lcintexpr (lcexpr Int))

(@debug-label dlb21)
(defdata nothing)

(@debug-label dlb22)
(deftype purelcexpr (lcexpr nothing)))

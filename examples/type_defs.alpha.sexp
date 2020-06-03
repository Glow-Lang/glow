(@module
(defdata yn Yes No)

(defdata ordering LT EQ GT)

(defdata pos2d (Posn int int))

(deftype colorRGB (@record (r int) (g int) (b int)))

(defdata (pair 'a 'b) (Pair 'a 'b))

(def pair_tuple
  (λ ((p : (pair 'a 'b))) : (@tuple 'a 'b)
    (switch p
      ((Pair a b) (@tuple a b)))))

(def tuple_pair
  (λ ((t : (@tuple 'a 'b))) : (pair 'a 'b)
    (switch t
      ((@tuple a0 b0) (@app Pair a0 b0)))))

(defdata (option 'a) (Some 'a) None)

(defdata (result 'a 'b) (Ok 'a) (Error 'b))

(def option_result
  (λ ((o : (option 'a))) : (result 'a (@tuple))
    (switch o
      ((Some a1) (@app Ok a1))
      (None (@app Error (@tuple))))))

(defdata natural
  Zero
  (Succ natural))

(defdata (conslist 'a)
  Empty
  (Cons 'a (conslist 'a)))

(deftype (assocpairlist 'a 'b) (conslist (pair 'a 'b)))

(defdata (lcexpr 'lit)
  (Lit 'lit)
  (Var natural)
  (Lam (lcexpr 'lit))
  (App (lcexpr 'lit) (lcexpr 'lit)))

(deftype lcintexpr (lcexpr int))

(defdata nothing)

(deftype purelcexpr (lcexpr nothing)))

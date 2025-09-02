(hash (EQ (type:name 'ordering0))
      (Error (type:arrow
              [(type:var 'bₚ0)]
              (type:app
               (type:name 'result0)
               [[ntype:top ptype:bottom] [ntype:top (type:var 'bₚ0)]])))
      (GT (type:name 'ordering0))
      (LT (type:name 'ordering0))
      (No (type:name 'yn0))
      (Ok (type:arrow
           [(type:var 'aₚ1)]
           (type:app
            (type:name 'result0)
            [[ntype:top (type:var 'aₚ1)] [ntype:top ptype:bottom]])))
      (Pair (type:arrow
             [(type:var 'aₚ) (type:var 'bₚ)]
             (type:app
              (type:name 'pair0)
              [[ntype:top (type:var 'aₚ)] [ntype:top (type:var 'bₚ)]])))
      (Yes (type:name 'yn0))
      (a (type:var 'a2))
      (a0 (type:var 'a4))
      (a1 (type:var 'a6))
      (b (type:var 'b1))
      (b0 (type:var 'b3))
      (conslist1 (type:record (symdict)))
      (lcexpr1 (type:record (symdict)))
      (natural1
       (type:record
        (symdict ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'natural0))))))
      (nothing1
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'nothing0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'nothing0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'nothing0))))))
      (o (type:app (type:name 'option0) [[(type:var 'a) (type:var 'a)]]))
      (option1 (type:record (symdict)))
      (option_result
       (type:arrow
        [(type:app (type:name 'option0) [[(type:var 'a) (type:var 'a)]])]
        (type:app
         (type:name 'result0)
         [[(type:var 'a) (type:var 'a)] [(type:tuple []) (type:tuple [])]])))
      (ordering1
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'ordering0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'ordering0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'ordering0))))))
      (p (type:app
          (type:name 'pair0)
          [[(type:var 'a) (type:var 'a)] [(type:var 'b) (type:var 'b)]]))
      (pair1 (type:record (symdict)))
      (pair_tuple
       (type:arrow
        [(type:app
          (type:name 'pair0)
          [[(type:var 'a) (type:var 'a)] [(type:var 'b) (type:var 'b)]])]
        (type:tuple [(type:var 'a) (type:var 'b)])))
      (pos2d1 (type:record
               (symdict ('input
                         (type:arrow
                          [(type:name 'Bytes)]
                          (type:name 'pos2d0))))))
      (result1 (type:record (symdict)))
      (t (type:tuple [(type:var 'a) (type:var 'b)]))
      (tag (type:name 'Bytes))
      (tag0 (type:name 'Bytes))
      (tag1 (type:name 'Bytes))
      (tag2 (type:name 'Bytes))
      (tag3 (type:name 'Bytes))
      (tuple_pair
       (type:arrow
        [(type:tuple [(type:var 'a) (type:var 'b)])]
        (type:app
         (type:name 'pair0)
         [[(type:var 'a) (type:var 'a)] [(type:var 'b) (type:var 'b)]])))
      (x (type:name 'yn0))
      (x0 (type:name-subtype 'Nat (type:name 'Int)))
      (x1 (type:name 'yn0))
      (x2 (type:name 'ordering0))
      (x3 (type:name-subtype 'Nat (type:name 'Int)))
      (x4 (type:name 'ordering0))
      (x5 (type:name 'pos2d0))
      (x6 (type:name 'natural0))
      (x7 (type:name 'nothing0))
      (x8 (type:name-subtype 'Nat (type:name 'Int)))
      (x9 (type:name 'nothing0))
      (yn1 (type:record
            (symdict ('toNat
                      (type:arrow
                       [(type:name 'yn0)]
                       (type:name-subtype 'Nat (type:name 'Int))))
                     ('ofNat
                      (type:arrow
                       [(type:name-subtype 'Nat (type:name 'Int))]
                       (type:name 'yn0)))
                     ('input
                      (type:arrow [(type:name 'Bytes)] (type:name 'yn0))))))
      (0 (type:name-subtype 'Nat (type:name 'Int)))
      (1 (type:name-subtype 'Nat (type:name 'Int)))
      (2 (type:name-subtype 'Nat (type:name 'Int)))
      ((@app Error (@tuple))
       (type:app
        (type:name 'result0)
        [[ntype:top ptype:bottom] [ntype:top ptype:bottom]]))
      ((@app Ok a1)
       (type:app
        (type:name 'result0)
        [[ntype:top (type:var 'a6)] [ntype:top ptype:bottom]]))
      ((@app Pair a0 b0)
       (type:app
        (type:name 'pair0)
        [[ntype:top (type:var 'a4)] [ntype:top (type:var 'b3)]]))
      ((@record) (type:record (symdict)))
      ((@record (input (λ (tag) (: yn) (def x (: yn) (input yn tag)) x))
                (toNat (λ ((x1 : yn))
                          (: Nat)
                          (switch x1 ((@app-ctor Yes) 0) ((@app-ctor No) 1))))
                (ofNat (λ ((x0 : Nat)) (: yn) (switch x0 (0 Yes) (1 No)))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'yn0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'yn0)))
                 ('input (type:arrow [(type:name 'Bytes)] (type:name 'yn0))))))
      ((@record (input (λ (tag) (def x (input yn tag)) x))
                (toNat (λ (x1)
                          (switch x1 ((@app-ctor Yes) 0) ((@app-ctor No) 1))))
                (ofNat (λ (x0) (switch x0 (0 Yes) (1 No)))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'yn0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'yn0)))
                 ('input (type:arrow [(type:name 'Bytes)] (type:name 'yn0))))))
      ((@record (input (λ (tag0)
                          (: ordering)
                          (def x2 (: ordering) (input ordering tag0))
                          x2))
                (toNat (λ ((x4 : ordering))
                          (: Nat)
                          (switch x4
                                  ((@app-ctor LT) 0)
                                  ((@app-ctor EQ) 1)
                                  ((@app-ctor GT) 2))))
                (ofNat (λ ((x3 : Nat))
                          (: ordering)
                          (switch x3 (0 LT) (1 EQ) (2 GT)))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'ordering0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'ordering0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'ordering0))))))
      ((@record (input (λ (tag0) (def x2 (input ordering tag0)) x2))
                (toNat (λ (x4)
                          (switch x4
                                  ((@app-ctor LT) 0)
                                  ((@app-ctor EQ) 1)
                                  ((@app-ctor GT) 2))))
                (ofNat (λ (x3) (switch x3 (0 LT) (1 EQ) (2 GT)))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'ordering0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'ordering0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'ordering0))))))
      ((@record (input (λ (tag1)
                          (: pos2d)
                          (def x5 (: pos2d) (input pos2d tag1))
                          x5)))
       (type:record
        (symdict ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'pos2d0))))))
      ((@record (input (λ (tag1) (def x5 (input pos2d tag1)) x5)))
       (type:record
        (symdict ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'pos2d0))))))
      ((@record (input (λ (tag2)
                          (: natural)
                          (def x6 (: natural) (input natural tag2))
                          x6)))
       (type:record
        (symdict ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'natural0))))))
      ((@record (input (λ (tag2) (def x6 (input natural tag2)) x6)))
       (type:record
        (symdict ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'natural0))))))
      ((@record (input (λ (tag3)
                          (: nothing)
                          (def x7 (: nothing) (input nothing tag3))
                          x7))
                (toNat (λ ((x9 : nothing)) (: Nat) (switch x9)))
                (ofNat (λ ((x8 : Nat)) (: nothing) (switch x8))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'nothing0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'nothing0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'nothing0))))))
      ((@record (input (λ (tag3) (def x7 (input nothing tag3)) x7))
                (toNat (λ (x9) (switch x9)))
                (ofNat (λ (x8) (switch x8))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'nothing0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'nothing0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'nothing0))))))
      ((@tuple) (type:tuple []))
      ((@tuple a b) (type:tuple [(type:var 'a2) (type:var 'b1)]))
      ((input natural tag2) (type:name 'natural0))
      ((input nothing tag3) (type:name 'nothing0))
      ((input ordering tag0) (type:name 'ordering0))
      ((input pos2d tag1) (type:name 'pos2d0))
      ((input yn tag) (type:name 'yn0))
      ((switch o
               ((@app-ctor Some (@var-pat a1))
                (@debug-label dlb14)
                (@app Ok a1))
               ((@app-ctor None) (@debug-label dlb15) (@app Error (@tuple))))
       (type:app
        (type:name 'result0)
        [[ntype:top ptype:bottom] [ntype:top ptype:bottom]]))
      ((switch p
               ((@app-ctor Pair (@var-pat a) (@var-pat b))
                (@debug-label dlb6)
                (@tuple a b)))
       (type:tuple [ptype:bottom ptype:bottom]))
      ((switch t
               ((@tuple (@var-pat a0) (@var-pat b0))
                (@debug-label dlb9)
                (@app Pair a0 b0)))
       (type:app
        (type:name 'pair0)
        [[ntype:top ptype:bottom] [ntype:top ptype:bottom]]))
      ((switch x0 (0 Yes) (1 No)) (type:name 'yn0))
      ((switch x1 ((@app-ctor Yes) 0) ((@app-ctor No) 1))
       (type:name-subtype 'Nat (type:name 'Int)))
      ((switch x3 (0 LT) (1 EQ) (2 GT)) (type:name 'ordering0))
      ((switch x4 ((@app-ctor LT) 0) ((@app-ctor EQ) 1) ((@app-ctor GT) 2))
       (type:name-subtype 'Nat (type:name 'Int)))
      ((switch x8) ptype:bottom)
      ((switch x9) ptype:bottom)
      ((λ (o)
          (@debug-label dlb13)
          (switch o
                  ((@app-ctor Some (@var-pat a1))
                   (@debug-label dlb14)
                   (@app Ok a1))
                  ((@app-ctor None)
                   (@debug-label dlb15)
                   (@app Error (@tuple)))))
       (type:arrow
        [(type:app (type:name 'option0) [[(type:var 'a) (type:var 'a)]])]
        (type:app
         (type:name 'result0)
         [[(type:var 'a) (type:var 'a)] [(type:tuple []) (type:tuple [])]])))
      ((λ (p)
          (@debug-label dlb5)
          (switch p
                  ((@app-ctor Pair (@var-pat a) (@var-pat b))
                   (@debug-label dlb6)
                   (@tuple a b))))
       (type:arrow
        [(type:app
          (type:name 'pair0)
          [[(type:var 'a) (type:var 'a)] [(type:var 'b) (type:var 'b)]])]
        (type:tuple [(type:var 'a) (type:var 'b)])))
      ((λ (t)
          (@debug-label dlb8)
          (switch t
                  ((@tuple (@var-pat a0) (@var-pat b0))
                   (@debug-label dlb9)
                   (@app Pair a0 b0))))
       (type:arrow
        [(type:tuple [(type:var 'a) (type:var 'b)])]
        (type:app
         (type:name 'pair0)
         [[(type:var 'a) (type:var 'a)] [(type:var 'b) (type:var 'b)]])))
      ((λ (tag) (: yn) (def x (: yn) (input yn tag)) x)
       (type:arrow [(type:name 'Bytes)] (type:name 'yn0)))
      ((λ (tag) (def x (input yn tag)) x)
       (type:arrow [(type:name 'Bytes)] (type:name 'yn0)))
      ((λ (tag0) (: ordering) (def x2 (: ordering) (input ordering tag0)) x2)
       (type:arrow [(type:name 'Bytes)] (type:name 'ordering0)))
      ((λ (tag0) (def x2 (input ordering tag0)) x2)
       (type:arrow [(type:name 'Bytes)] (type:name 'ordering0)))
      ((λ (tag1) (: pos2d) (def x5 (: pos2d) (input pos2d tag1)) x5)
       (type:arrow [(type:name 'Bytes)] (type:name 'pos2d0)))
      ((λ (tag1) (def x5 (input pos2d tag1)) x5)
       (type:arrow [(type:name 'Bytes)] (type:name 'pos2d0)))
      ((λ (tag2) (: natural) (def x6 (: natural) (input natural tag2)) x6)
       (type:arrow [(type:name 'Bytes)] (type:name 'natural0)))
      ((λ (tag2) (def x6 (input natural tag2)) x6)
       (type:arrow [(type:name 'Bytes)] (type:name 'natural0)))
      ((λ (tag3) (: nothing) (def x7 (: nothing) (input nothing tag3)) x7)
       (type:arrow [(type:name 'Bytes)] (type:name 'nothing0)))
      ((λ (tag3) (def x7 (input nothing tag3)) x7)
       (type:arrow [(type:name 'Bytes)] (type:name 'nothing0)))
      ((λ (x0) (switch x0 (0 Yes) (1 No)))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'yn0)))
      ((λ (x1) (switch x1 ((@app-ctor Yes) 0) ((@app-ctor No) 1)))
       (type:arrow
        [(type:name 'yn0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((λ (x3) (switch x3 (0 LT) (1 EQ) (2 GT)))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'ordering0)))
      ((λ (x4)
          (switch x4 ((@app-ctor LT) 0) ((@app-ctor EQ) 1) ((@app-ctor GT) 2)))
       (type:arrow
        [(type:name 'ordering0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((λ (x8) (switch x8))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'nothing0)))
      ((λ (x9) (switch x9))
       (type:arrow
        [(type:name 'nothing0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((λ ((o : (option 'a)))
          (: (result 'a (@tuple)))
          (@debug-label dlb13)
          (switch o
                  ((@app-ctor Some (@var-pat a1))
                   (@debug-label dlb14)
                   (@app Ok a1))
                  ((@app-ctor None)
                   (@debug-label dlb15)
                   (@app Error (@tuple)))))
       (type:arrow
        [(type:app (type:name 'option0) [[(type:var 'a) (type:var 'a)]])]
        (type:app
         (type:name 'result0)
         [[(type:var 'a) (type:var 'a)] [(type:tuple []) (type:tuple [])]])))
      ((λ ((p : (pair 'a 'b)))
          (: (@tuple 'a 'b))
          (@debug-label dlb5)
          (switch p
                  ((@app-ctor Pair (@var-pat a) (@var-pat b))
                   (@debug-label dlb6)
                   (@tuple a b))))
       (type:arrow
        [(type:app
          (type:name 'pair0)
          [[(type:var 'a) (type:var 'a)] [(type:var 'b) (type:var 'b)]])]
        (type:tuple [(type:var 'a) (type:var 'b)])))
      ((λ ((t : (@tuple 'a 'b)))
          (: (pair 'a 'b))
          (@debug-label dlb8)
          (switch t
                  ((@tuple (@var-pat a0) (@var-pat b0))
                   (@debug-label dlb9)
                   (@app Pair a0 b0))))
       (type:arrow
        [(type:tuple [(type:var 'a) (type:var 'b)])]
        (type:app
         (type:name 'pair0)
         [[(type:var 'a) (type:var 'a)] [(type:var 'b) (type:var 'b)]])))
      ((λ ((x0 : Nat)) (: yn) (switch x0 (0 Yes) (1 No)))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'yn0)))
      ((λ ((x1 : yn))
          (: Nat)
          (switch x1 ((@app-ctor Yes) 0) ((@app-ctor No) 1)))
       (type:arrow
        [(type:name 'yn0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((λ ((x3 : Nat)) (: ordering) (switch x3 (0 LT) (1 EQ) (2 GT)))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'ordering0)))
      ((λ ((x4 : ordering))
          (: Nat)
          (switch x4 ((@app-ctor LT) 0) ((@app-ctor EQ) 1) ((@app-ctor GT) 2)))
       (type:arrow
        [(type:name 'ordering0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((λ ((x8 : Nat)) (: nothing) (switch x8))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'nothing0)))
      ((λ ((x9 : nothing)) (: Nat) (switch x9))
       (type:arrow
        [(type:name 'nothing0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((0 LT)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'ordering0)])))
      ((0 Yes)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'yn0)])))
      ((1 EQ)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'ordering0)])))
      ((1 No)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'yn0)])))
      ((2 GT)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'ordering0)])))
      (((@app-ctor EQ) 1)
       (type:arrow
        [(type:name 'ordering0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@app-ctor GT) 2)
       (type:arrow
        [(type:name 'ordering0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@app-ctor LT) 0)
       (type:arrow
        [(type:name 'ordering0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@app-ctor No) 1)
       (type:arrow
        [(type:name 'yn0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@app-ctor None) (@debug-label dlb15) (@app Error (@tuple)))
       (type:arrow
        [(type:app (type:name 'option0) [[ptype:bottom ntype:top]])]
        (type:tuple
         [(type:record (symdict))
          (type:app
           (type:name 'result0)
           [[ntype:top ptype:bottom] [ntype:top ptype:bottom]])])))
      (((@app-ctor Pair (@var-pat a) (@var-pat b))
        (@debug-label dlb6)
        (@tuple a b))
       (type:arrow
        [(type:app
          (type:name 'pair0)
          [[ptype:bottom (ntype:intersection [(type:var 'a2) (type:var 'a3)])]
           [ptype:bottom
            (ntype:intersection [(type:var 'b1) (type:var 'b2)])]])]
        (type:tuple
         [(type:record (symdict ('a (type:var 'a2)) ('b (type:var 'b1))))
          (type:tuple [(type:var 'a3) (type:var 'b2)])])))
      (((@app-ctor Some (@var-pat a1)) (@debug-label dlb14) (@app Ok a1))
       (type:arrow
        [(type:app
          (type:name 'option0)
          [[ptype:bottom
            (ntype:intersection [(type:var 'a6) (type:var 'aₚ1)])]])]
        (type:tuple
         [(type:record (symdict ('a1 (type:var 'a6))))
          (type:app
           (type:name 'result0)
           [[ntype:top (type:var 'aₚ1)] [ntype:top ptype:bottom]])])))
      (((@app-ctor Yes) 0)
       (type:arrow
        [(type:name 'yn0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@tuple (@var-pat a0) (@var-pat b0))
        (@debug-label dlb9)
        (@app Pair a0 b0))
       (type:arrow
        [(type:tuple
          [(ntype:intersection [(type:var 'a4) (type:var 'aₚ)])
           (ntype:intersection [(type:var 'b3) (type:var 'bₚ)])])]
        (type:tuple
         [(type:record (symdict ('b0 (type:var 'b3)) ('a0 (type:var 'a4))))
          (type:app
           (type:name 'pair0)
           [[ntype:top (type:var 'aₚ)] [ntype:top (type:var 'bₚ)]])]))))

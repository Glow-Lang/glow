(hash (* (type:arrow [(type:name 'Int) (type:name 'Int)] (type:name 'Int)))
      (+ (type:arrow [(type:name 'Int) (type:name 'Int)] (type:name 'Int)))
      (- (type:arrow [(type:name 'Int) (type:name 'Int)] (type:name 'Int)))
      (A (type:name 'Participant))
      (A_Wins (type:name 'Outcome0))
      (B (type:name 'Participant))
      (B_Wins (type:name 'Outcome0))
      (Draw (type:name 'Outcome0))
      (Hand1 (type:record
              (symdict ('toNat
                        (type:arrow
                         [(type:name 'Hand0)]
                         (type:name-subtype 'Nat (type:name 'Int))))
                       ('ofNat
                        (type:arrow
                         [(type:name-subtype 'Nat (type:name 'Int))]
                         (type:name 'Hand0)))
                       ('input
                        (type:arrow
                         [(type:name 'Bytes)]
                         (type:name 'Hand0))))))
      (Outcome1
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'Outcome0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'Outcome0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'Outcome0))))))
      (Paper (type:name 'Hand0))
      (Rock (type:name 'Hand0))
      (Scissors (type:name 'Hand0))
      (commitment (type:name 'Digest))
      (handA (type:name 'Hand0))
      (handA0 (type:name 'Hand0))
      (handB (type:name 'Hand0))
      (handB0 (type:name 'Hand0))
      (mod (type:arrow
            [(type:name 'Int) (type:name-subtype 'Nat (type:name 'Int))]
            (type:name-subtype 'Nat (type:name 'Int))))
      (outcome (type:name 'Outcome0))
      (randomUInt256 (type:arrow [] (type:name-subtype 'Nat (type:name 'Int))))
      (rockPaperScissors
       (type:arrow
        [(type:name 'Participant) (type:name 'Participant) (type:name 'Int)]
        (type:name 'Outcome0)))
      (salt (type:name-subtype 'Nat (type:name 'Int)))
      (tag (type:name 'Bytes))
      (tag0 (type:name 'Bytes))
      (wagerAmount (type:name 'Int))
      (winner (type:arrow
               [(type:name 'Hand0) (type:name 'Hand0)]
               (type:name 'Outcome0)))
      (x (type:name 'Hand0))
      (x0 (type:name-subtype 'Nat (type:name 'Int)))
      (x1 (type:name 'Hand0))
      (x2 (type:name 'Outcome0))
      (x3 (type:name-subtype 'Nat (type:name 'Int)))
      (x4 (type:name 'Outcome0))
      (0 (type:name-subtype 'Nat (type:name 'Int)))
      (1 (type:name-subtype 'Nat (type:name 'Int)))
      (2 (type:name-subtype 'Nat (type:name 'Int)))
      (3 (type:name-subtype 'Nat (type:name 'Int)))
      (4 (type:name-subtype 'Nat (type:name 'Int)))
      ("First player, pick your hand" (type:name 'Bytes))
      ("Second player, pick your hand" (type:name 'Bytes))
      ((== commitment (digest (@tuple salt handA0))) (type:name 'Bool))
      ((@app * 2 wagerAmount) (type:name 'Int))
      ((@app +
             (@app (@dot Hand1 toNat) handA)
             (@app - 4 (@app (@dot Hand1 toNat) handB)))
       (type:name 'Int))
      ((@app +
             (@app (@dot/type Hand toNat) handA)
             (@app - 4 (@app (@dot/type Hand toNat) handB)))
       (type:name 'Int))
      ((@app - 4 (@app (@dot Hand1 toNat) handB)) (type:name 'Int))
      ((@app - 4 (@app (@dot/type Hand toNat) handB)) (type:name 'Int))
      ((@app mod
             (@app +
                   (@app (@dot Hand1 toNat) handA)
                   (@app - 4 (@app (@dot Hand1 toNat) handB)))
             3)
       (type:name-subtype 'Nat (type:name 'Int)))
      ((@app mod
             (@app +
                   (@app (@dot/type Hand toNat) handA)
                   (@app - 4 (@app (@dot/type Hand toNat) handB)))
             3)
       (type:name-subtype 'Nat (type:name 'Int)))
      ((@app randomUInt256) (type:name-subtype 'Nat (type:name 'Int)))
      ((@app winner handA0 handB0) (type:name 'Outcome0))
      ((@app (@dot Hand1 input) "First player, pick your hand")
       (type:name 'Hand0))
      ((@app (@dot Hand1 input) "Second player, pick your hand")
       (type:name 'Hand0))
      ((@app (@dot Hand1 toNat) handA)
       (type:name-subtype 'Nat (type:name 'Int)))
      ((@app (@dot Hand1 toNat) handB)
       (type:name-subtype 'Nat (type:name 'Int)))
      ((@app (@dot Outcome1 ofNat)
             (@app mod
                   (@app +
                         (@app (@dot Hand1 toNat) handA)
                         (@app - 4 (@app (@dot Hand1 toNat) handB)))
                   3))
       (type:name 'Outcome0))
      ((@app (@dot/type Hand input) "First player, pick your hand")
       (type:name 'Hand0))
      ((@app (@dot/type Hand input) "Second player, pick your hand")
       (type:name 'Hand0))
      ((@app (@dot/type Hand toNat) handA)
       (type:name-subtype 'Nat (type:name 'Int)))
      ((@app (@dot/type Hand toNat) handB)
       (type:name-subtype 'Nat (type:name 'Int)))
      ((@app (@dot/type Outcome ofNat)
             (@app mod
                   (@app +
                         (@app (@dot/type Hand toNat) handA)
                         (@app - 4 (@app (@dot/type Hand toNat) handB)))
                   3))
       (type:name 'Outcome0))
      ((@dot Hand1 input) (type:arrow [(type:name 'Bytes)] (type:name 'Hand0)))
      ((@dot Hand1 toNat)
       (type:arrow
        [(type:name 'Hand0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((@dot Outcome1 ofNat)
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'Outcome0)))
      ((@dot/type Hand input)
       (type:arrow [(type:name 'Bytes)] (type:name 'Hand0)))
      ((@dot/type Hand toNat)
       (type:arrow
        [(type:name 'Hand0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((@dot/type Outcome ofNat)
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'Outcome0)))
      ((@make-interaction
        ((@record (participants [A B]) (assets [DefaultToken])))
        (wagerAmount)
        ()
        (@debug-label dlb4)
        (@ A (@debug-label dlb5))
        (@ A
           (def handA0
                ()
                (@app (@dot/type Hand input) "First player, pick your hand")))
        (@ A (@debug-label dlb6))
        (@ A (def salt () (@app randomUInt256)))
        (@debug-label dlb7)
        (@ A (def commitment () (digest (@tuple salt handA0))))
        (@debug-label dlb8)
        (publish! A commitment)
        (@debug-label dlb9)
        (deposit! A (@record (DefaultToken wagerAmount)))
        (@debug-label dlb10)
        (@ B
           (def handB0
                ()
                (@app (@dot/type Hand input) "Second player, pick your hand")))
        (@debug-label dlb11)
        (publish! B handB0)
        (@debug-label dlb12)
        (deposit! B (@record (DefaultToken wagerAmount)))
        (@debug-label dlb13)
        (publish! A salt)
        (publish! A handA0)
        (@debug-label dlb14)
        (require! (== commitment (digest (@tuple salt handA0))))
        (@debug-label dlb15)
        (def outcome () (@app winner handA0 handB0))
        (@debug-label dlb16)
        (switch outcome
                ((@app-ctor A_Wins)
                 (@debug-label dlb17)
                 (withdraw! A (@record (DefaultToken (@app * 2 wagerAmount)))))
                ((@app-ctor B_Wins)
                 (@debug-label dlb18)
                 (withdraw! B (@record (DefaultToken (@app * 2 wagerAmount)))))
                ((@app-ctor Draw)
                 (@debug-label dlb19)
                 (withdraw! A (@record (DefaultToken wagerAmount)))
                 (@debug-label dlb20)
                 (withdraw! B (@record (DefaultToken wagerAmount)))))
        (@debug-label dlb21)
        outcome)
       (type:arrow
        [(type:name 'Participant) (type:name 'Participant) (type:name 'Int)]
        (type:name 'Outcome0)))
      ((@make-interaction
        ((@record (participants [A B]) (assets [DefaultToken])))
        (wagerAmount)
        (@debug-label dlb4)
        (@ A (@debug-label dlb5))
        (@ A
           (def handA0
                (@app (@dot Hand1 input) "First player, pick your hand")))
        (@ A (@debug-label dlb6))
        (@ A (def salt (@app randomUInt256)))
        (@debug-label dlb7)
        (@ A (def commitment (digest (@tuple salt handA0))))
        (@debug-label dlb8)
        (publish! A commitment)
        (@debug-label dlb9)
        (deposit! A (@record (DefaultToken wagerAmount)))
        (@debug-label dlb10)
        (@ B
           (def handB0
                (@app (@dot Hand1 input) "Second player, pick your hand")))
        (@debug-label dlb11)
        (publish! B handB0)
        (@debug-label dlb12)
        (deposit! B (@record (DefaultToken wagerAmount)))
        (@debug-label dlb13)
        (publish! A salt)
        (publish! A handA0)
        (@debug-label dlb14)
        (require! (== commitment (digest (@tuple salt handA0))))
        (@debug-label dlb15)
        (def outcome (@app winner handA0 handB0))
        (@debug-label dlb16)
        (switch outcome
                ((@app-ctor A_Wins)
                 (@debug-label dlb17)
                 (withdraw! A (@record (DefaultToken (@app * 2 wagerAmount)))))
                ((@app-ctor B_Wins)
                 (@debug-label dlb18)
                 (withdraw! B (@record (DefaultToken (@app * 2 wagerAmount)))))
                ((@app-ctor Draw)
                 (@debug-label dlb19)
                 (withdraw! A (@record (DefaultToken wagerAmount)))
                 (@debug-label dlb20)
                 (withdraw! B (@record (DefaultToken wagerAmount)))))
        (@debug-label dlb21)
        outcome)
       (type:arrow
        [(type:name 'Participant) (type:name 'Participant) (type:name 'Int)]
        (type:name 'Outcome0)))
      ((@record (input (λ (tag) (: Hand) (def x (: Hand) (input Hand tag)) x))
                (toNat (λ ((x1 : Hand))
                          (: Nat)
                          (switch x1
                                  ((@app-ctor Rock) 0)
                                  ((@app-ctor Paper) 1)
                                  ((@app-ctor Scissors) 2))))
                (ofNat (λ ((x0 : Nat))
                          (: Hand)
                          (switch x0 (0 Rock) (1 Paper) (2 Scissors)))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'Hand0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'Hand0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'Hand0))))))
      ((@record (input (λ (tag) (def x (input Hand tag)) x))
                (toNat (λ (x1)
                          (switch x1
                                  ((@app-ctor Rock) 0)
                                  ((@app-ctor Paper) 1)
                                  ((@app-ctor Scissors) 2))))
                (ofNat (λ (x0) (switch x0 (0 Rock) (1 Paper) (2 Scissors)))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'Hand0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'Hand0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'Hand0))))))
      ((@record (input (λ (tag0)
                          (: Outcome)
                          (def x2 (: Outcome) (input Outcome tag0))
                          x2))
                (toNat (λ ((x4 : Outcome))
                          (: Nat)
                          (switch x4
                                  ((@app-ctor B_Wins) 0)
                                  ((@app-ctor Draw) 1)
                                  ((@app-ctor A_Wins) 2))))
                (ofNat (λ ((x3 : Nat))
                          (: Outcome)
                          (switch x3 (0 B_Wins) (1 Draw) (2 A_Wins)))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'Outcome0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'Outcome0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'Outcome0))))))
      ((@record (input (λ (tag0) (def x2 (input Outcome tag0)) x2))
                (toNat (λ (x4)
                          (switch x4
                                  ((@app-ctor B_Wins) 0)
                                  ((@app-ctor Draw) 1)
                                  ((@app-ctor A_Wins) 2))))
                (ofNat (λ (x3) (switch x3 (0 B_Wins) (1 Draw) (2 A_Wins)))))
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'Outcome0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'Outcome0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'Outcome0))))))
      ((@tuple salt handA0)
       (type:tuple
        [(type:name-subtype 'Nat (type:name 'Int)) (type:name 'Hand0)]))
      ((deposit! A (@record (DefaultToken wagerAmount))) (type:tuple []))
      ((deposit! B (@record (DefaultToken wagerAmount))) (type:tuple []))
      ((digest (@tuple salt handA0)) (type:name 'Digest))
      ((input Hand tag) (type:name 'Hand0))
      ((input Outcome tag0) (type:name 'Outcome0))
      ((require! (== commitment (digest (@tuple salt handA0))))
       (type:tuple []))
      ((switch outcome
               ((@app-ctor A_Wins)
                (@debug-label dlb17)
                (withdraw! A (@record (DefaultToken (@app * 2 wagerAmount)))))
               ((@app-ctor B_Wins)
                (@debug-label dlb18)
                (withdraw! B (@record (DefaultToken (@app * 2 wagerAmount)))))
               ((@app-ctor Draw)
                (@debug-label dlb19)
                (withdraw! A (@record (DefaultToken wagerAmount)))
                (@debug-label dlb20)
                (withdraw! B (@record (DefaultToken wagerAmount)))))
       (type:tuple []))
      ((switch x0 (0 Rock) (1 Paper) (2 Scissors)) (type:name 'Hand0))
      ((switch x1
               ((@app-ctor Rock) 0)
               ((@app-ctor Paper) 1)
               ((@app-ctor Scissors) 2))
       (type:name-subtype 'Nat (type:name 'Int)))
      ((switch x3 (0 B_Wins) (1 Draw) (2 A_Wins)) (type:name 'Outcome0))
      ((switch x4
               ((@app-ctor B_Wins) 0)
               ((@app-ctor Draw) 1)
               ((@app-ctor A_Wins) 2))
       (type:name-subtype 'Nat (type:name 'Int)))
      ((withdraw! A (@record (DefaultToken wagerAmount))) (type:tuple []))
      ((withdraw! A (@record (DefaultToken (@app * 2 wagerAmount))))
       (type:tuple []))
      ((withdraw! B (@record (DefaultToken wagerAmount))) (type:tuple []))
      ((withdraw! B (@record (DefaultToken (@app * 2 wagerAmount))))
       (type:tuple []))
      ((λ (handA handB)
          (@debug-label dlb2)
          (@app (@dot Outcome1 ofNat)
                (@app mod
                      (@app +
                            (@app (@dot Hand1 toNat) handA)
                            (@app - 4 (@app (@dot Hand1 toNat) handB)))
                      3)))
       (type:arrow
        [(type:name 'Hand0) (type:name 'Hand0)]
        (type:name 'Outcome0)))
      ((λ (tag) (: Hand) (def x (: Hand) (input Hand tag)) x)
       (type:arrow [(type:name 'Bytes)] (type:name 'Hand0)))
      ((λ (tag) (def x (input Hand tag)) x)
       (type:arrow [(type:name 'Bytes)] (type:name 'Hand0)))
      ((λ (tag0) (: Outcome) (def x2 (: Outcome) (input Outcome tag0)) x2)
       (type:arrow [(type:name 'Bytes)] (type:name 'Outcome0)))
      ((λ (tag0) (def x2 (input Outcome tag0)) x2)
       (type:arrow [(type:name 'Bytes)] (type:name 'Outcome0)))
      ((λ (x0) (switch x0 (0 Rock) (1 Paper) (2 Scissors)))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'Hand0)))
      ((λ (x1)
          (switch x1
                  ((@app-ctor Rock) 0)
                  ((@app-ctor Paper) 1)
                  ((@app-ctor Scissors) 2)))
       (type:arrow
        [(type:name 'Hand0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((λ (x3) (switch x3 (0 B_Wins) (1 Draw) (2 A_Wins)))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'Outcome0)))
      ((λ (x4)
          (switch x4
                  ((@app-ctor B_Wins) 0)
                  ((@app-ctor Draw) 1)
                  ((@app-ctor A_Wins) 2)))
       (type:arrow
        [(type:name 'Outcome0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((λ ((handA : Hand) (handB : Hand))
          (: Outcome)
          (@debug-label dlb2)
          (@app (@dot/type Outcome ofNat)
                (@app mod
                      (@app +
                            (@app (@dot/type Hand toNat) handA)
                            (@app - 4 (@app (@dot/type Hand toNat) handB)))
                      3)))
       (type:arrow
        [(type:name 'Hand0) (type:name 'Hand0)]
        (type:name 'Outcome0)))
      ((λ ((x0 : Nat)) (: Hand) (switch x0 (0 Rock) (1 Paper) (2 Scissors)))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'Hand0)))
      ((λ ((x1 : Hand))
          (: Nat)
          (switch x1
                  ((@app-ctor Rock) 0)
                  ((@app-ctor Paper) 1)
                  ((@app-ctor Scissors) 2)))
       (type:arrow
        [(type:name 'Hand0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((λ ((x3 : Nat)) (: Outcome) (switch x3 (0 B_Wins) (1 Draw) (2 A_Wins)))
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'Outcome0)))
      ((λ ((x4 : Outcome))
          (: Nat)
          (switch x4
                  ((@app-ctor B_Wins) 0)
                  ((@app-ctor Draw) 1)
                  ((@app-ctor A_Wins) 2)))
       (type:arrow
        [(type:name 'Outcome0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((0 B_Wins)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'Outcome0)])))
      ((0 Rock)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'Hand0)])))
      ((1 Draw)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'Outcome0)])))
      ((1 Paper)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'Hand0)])))
      ((2 A_Wins)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'Outcome0)])))
      ((2 Scissors)
       (type:arrow
        [(type:name 'Int)]
        (type:tuple [(type:record (symdict)) (type:name 'Hand0)])))
      (((@app-ctor A_Wins) 2)
       (type:arrow
        [(type:name 'Outcome0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@app-ctor A_Wins)
        (@debug-label dlb17)
        (withdraw! A (@record (DefaultToken (@app * 2 wagerAmount)))))
       (type:arrow
        [(type:name 'Outcome0)]
        (type:tuple [(type:record (symdict)) (type:tuple [])])))
      (((@app-ctor B_Wins) 0)
       (type:arrow
        [(type:name 'Outcome0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@app-ctor B_Wins)
        (@debug-label dlb18)
        (withdraw! B (@record (DefaultToken (@app * 2 wagerAmount)))))
       (type:arrow
        [(type:name 'Outcome0)]
        (type:tuple [(type:record (symdict)) (type:tuple [])])))
      (((@app-ctor Draw) 1)
       (type:arrow
        [(type:name 'Outcome0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@app-ctor Draw)
        (@debug-label dlb19)
        (withdraw! A (@record (DefaultToken wagerAmount)))
        (@debug-label dlb20)
        (withdraw! B (@record (DefaultToken wagerAmount))))
       (type:arrow
        [(type:name 'Outcome0)]
        (type:tuple [(type:record (symdict)) (type:tuple [])])))
      (((@app-ctor Paper) 1)
       (type:arrow
        [(type:name 'Hand0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@app-ctor Rock) 0)
       (type:arrow
        [(type:name 'Hand0)]
        (type:tuple
         [(type:record (symdict)) (type:name-subtype 'Nat (type:name 'Int))])))
      (((@app-ctor Scissors) 2)
       (type:arrow
        [(type:name 'Hand0)]
        (type:tuple
         [(type:record (symdict))
          (type:name-subtype 'Nat (type:name 'Int))]))))

(@module (defdata Hand Rock Paper Scissors)
         (def Hand1
              (@record (input (λ (tag) (def x (input Hand tag)) x))
                       (toNat (λ (x0)
                                 (switch x0 ((@app-ctor Rock) 0) ((@app-ctor Paper) 1) ((@app-ctor Scissors) 2))))
                       (ofNat (λ (x1)
                                 (switch x1 (0 Rock) (1 Paper) (2 Scissors))))))
         (defdata Outcome B_Wins Draw A_Wins)
         (def Outcome1
              (@record (input (λ (tag0) (def x2 (input Outcome tag0)) x2))
                       (toNat (λ (x3)
                                 (switch x3 ((@app-ctor B_Wins) 0) ((@app-ctor Draw) 1) ((@app-ctor A_Wins) 2))))
                       (ofNat (λ (x4)
                                 (switch x4 (0 B_Wins) (1 Draw) (2 A_Wins))))))
         (def winner
              (λ (handA handB)
                 (@app (@dot Outcome1 ofNat)
                       (@app mod
                             (@app +
                                   (@app (@dot Hand1 toNat) handA)
                                   (@app - 4 (@app (@dot Hand1 toNat) handB)))
                             3))))
         (def rockPaperScissors
              (@make-interaction
               ((@list A B))
               (wagerAmount)
               (@ A (def handA0 (@app (@dot Hand1 input) "First player, pick your hand")))
               (@ A (def salt (@app randomUInt256)))
               (@ A (def commitment (digest (@tuple salt handA0))))
               (publish! A commitment)
               (deposit! A wagerAmount)
               (@ B (def handB0 (@app (@dot Hand1 input) "Second player, pick your hand")))
               (publish! B handB0)
               (deposit! B wagerAmount)
               (publish! A salt)
               (publish! A handA0)
               (require! (== commitment (digest (@tuple salt handA0))))
               (def outcome (@app winner handA0 handB0))
               (switch outcome
                       ((@app-ctor A_Wins) (withdraw! A (@app * 2 wagerAmount)))
                       ((@app-ctor B_Wins) (withdraw! B (@app * 2 wagerAmount)))
                       ((@app-ctor Draw) (withdraw! A wagerAmount) (withdraw! B wagerAmount)))
               outcome)))

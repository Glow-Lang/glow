(defdata Hand
         Rock
         Paper
         Scissors
         with:
         (@record (input (λ (tag) (def x : Hand (input Hand tag)) x))
                  (toNat (λ ((x0 : Hand))
                            (switch x0 (Rock 0) (Paper 1) (Scissors 2))))
                  (ofNat (λ ((x1 : nat))
                            (switch x1 (0 Rock) (1 Paper) (2 Scissors))))))
(defdata Outcome
         B_Wins
         Draw
         A_Wins
         with:
         (@record (input (λ (tag0) (def x2 : Outcome (input Outcome tag0)) x2))
                  (toNat (λ ((x3 : Outcome))
                            (switch x3 (B_Wins 0) (Draw 1) (A_Wins 2))))
                  (ofNat (λ ((x4 : nat))
                            (switch x4 (0 B_Wins) (1 Draw) (2 A_Wins))))))
(def winner
     (λ ((handA : Hand) (handB : Hand))
        :
        Outcome
        (@app (@dot Outcome ofNat)
              (@app mod
                    (@app +
                          (@app (@dot Hand toNat) handA)
                          (@app - 4 (@app (@dot Hand toNat) handB)))
                    3))))
(@interaction
 ((@list A B))
 (def rockPaperScissors
      (λ (wagerAmount)
         (@ A
            (def handA0
                 (@app (@dot Hand input) "First player, pick your hand")))
         (@ A (def salt (@app randomUInt256)))
         (@ A (def commitment (digest (@tuple salt handA0))))
         (@ A (publish! commitment))
         (@ A (deposit! wagerAmount))
         (@ B
            (def handB0
                 (@app (@dot Hand input) "Second player, pick your hand")))
         (@ B (publish! handB0))
         (@ B (deposit! wagerAmount))
         (@ A (publish! salt handA0))
         (require! (== commitment (digest (@tuple salt handA0))))
         (def outcome (@app winner handA0 handB0))
         (switch outcome
                 (A_Wins (withdraw! A (@app * 2 wagerAmount)))
                 (B_Wins (withdraw! B (@app * 2 wagerAmount)))
                 (Draw (withdraw! A wagerAmount) (withdraw! B wagerAmount)))
         outcome)))

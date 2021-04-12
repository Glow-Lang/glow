(@module
(@debug-label dlb)
(defdata Hand
         Rock
         Paper
         Scissors
         with:
         (@record (input (λ (tag) (: Hand) (def x (: Hand) (input Hand tag)) x))
                  (toNat (λ ((x0 : Hand)) (: Nat) (switch x0 ((@app-ctor Rock) 0) ((@app-ctor Paper) 1) ((@app-ctor Scissors) 2))))
                  (ofNat (λ ((x1 : Nat)) (: Hand) (switch x1 (0 Rock) (1 Paper) (2 Scissors))))))
(@debug-label dlb0)
(defdata Outcome
         B_Wins
         Draw
         A_Wins
         with:
         (@record (input (λ (tag0) (: Outcome) (def x2 (: Outcome) (input Outcome tag0)) x2))
                  (toNat (λ ((x3 : Outcome)) (: Nat) (switch x3 ((@app-ctor B_Wins) 0) ((@app-ctor Draw) 1) ((@app-ctor A_Wins) 2))))
                  (ofNat (λ ((x4 : Nat)) (: Outcome) (switch x4 (0 B_Wins) (1 Draw) (2 A_Wins))))))

(@debug-label dlb1)
(def winner
     ()
     (λ ((handA : Hand) (handB : Hand))
        (: Outcome)
        (@debug-label dlb2)
        (@app (@dot/type Outcome ofNat)
              (@app mod (@app + (@app (@dot/type Hand toNat) handA) (@app - 4 (@app (@dot/type Hand toNat) handB))) 3))))

(@debug-label dlb3)
(def rockPaperScissors
     ()
     (@make-interaction
      ((@record (participants (@list A B)) (assets (@list DefaultToken))))
      (wagerAmount)
      ()
      (@debug-label dlb4)
      (@ A (def handA0 () (@app (@dot/type Hand input) "First player, pick your hand")))
      (@debug-label dlb5)
      (@ A (def salt () (@app randomUInt256)))
      (@debug-label dlb6)
      (@ A (def commitment () (digest (@tuple salt handA0))))
      (@debug-label dlb7)
      (publish! A commitment)
      (@debug-label dlb8)
      (deposit! A (@record (DefaultToken wagerAmount)))

      (@debug-label dlb9)
      (@ B (def handB0 () (@app (@dot/type Hand input) "Second player, pick your hand")))
      (@debug-label dlb10)
      (publish! B handB0)
      (@debug-label dlb11)
      (deposit! B (@record (DefaultToken wagerAmount)))

      (@debug-label dlb12)
      (publish! A salt)
      (publish! A handA0)
      (@debug-label dlb13)
      (require! (== commitment (digest (@tuple salt handA0))))
      (@debug-label dlb14)
      (def outcome () (@app winner handA0 handB0))

      (@debug-label dlb15)
      (switch outcome
              ((@app-ctor A_Wins) (@debug-label dlb16)
                                  (withdraw! A (@record (DefaultToken (@app * 2 wagerAmount)))))
              ((@app-ctor B_Wins) (@debug-label dlb17)
                                  (withdraw! B (@record (DefaultToken (@app * 2 wagerAmount)))))
              ((@app-ctor Draw) (@debug-label dlb18)
                                (withdraw! A (@record (DefaultToken wagerAmount)))
                                (@debug-label dlb19)
                                (withdraw! B (@record (DefaultToken wagerAmount)))))

      (@debug-label dlb20)
      outcome)))

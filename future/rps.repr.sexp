(deftype Hand
  (@sum (@tuple) (@tuple) (@tuple)))
(def Rock (@variant 0 (@tuple)))
(def Paper (@variant 1 (@tuple)))
(def Scissors (@variant 2 (@tuple)))
(def Hand_methods
  (@tuple (λ ((x0 : Hand)) (: nat)
            (switch x0 ((@variant 0 (@tuple)) 0) ((@variant 1 (@tuple)) 1) ((@variant 2 (@tuple)) 2)))
          (λ ((x1 : nat)) (: Hand)
            (switch x1 (0 Rock) (1 Paper) (2 Scissors)))
          (λ (tag) (: Hand) (def x (: Hand) (input Hand tag)) x)))
(defdata Outcome
         B_Wins
         Draw
         A_Wins
         with:
         (@record (input (λ (tag0) (: Outcome) (def x2 (: Outcome) (input Outcome tag0)) x2))
                  (toNat (λ ((x3 : Outcome)) (: nat) (switch x3 (B_Wins 0) (Draw 1) (A_Wins 2))))
                  (ofNat (λ ((x4 : nat)) (: Outcome) (switch x4 (0 B_Wins) (1 Draw) (2 A_Wins))))))
(def winner
     ()
     (λ ((handA : Hand) (handB : Hand))
        (: Outcome)
        (@app (@dot Outcome ofNat)
              (@app mod (@app + (@app (@dot Hand toNat) handA) (@app - 4 (@app (@dot Hand toNat) handB))) 3))))
(def rockPaperScissors
     ()
     (@make-interaction
      ((@list A B))
      (wagerAmount)
      ()
      (@ A (def handA0 () (@app (@dot Hand input) "First player, pick your hand")))
      (@ A (def salt () (@app randomUInt256)))
      (@ A (def commitment () (digest (@tuple salt handA0))))
      (@ A (publish! commitment))
      (@ A (deposit! wagerAmount))
      (@ B (def handB0 () (@app (@dot Hand input) "Second player, pick your hand")))
      (@ B (publish! handB0))
      (@ B (deposit! wagerAmount))
      (@ A (publish! salt handA0))
      (require! (== commitment (digest (@tuple salt handA0))))
      (def outcome () (@app winner handA0 handB0))
      (switch outcome
              (A_Wins (withdraw! A (@app * 2 wagerAmount)))
              (B_Wins (withdraw! B (@app * 2 wagerAmount)))
              (Draw (withdraw! A wagerAmount) (withdraw! B wagerAmount)))
      outcome))

(defdata Hand Rock Paper Scissors)

(defdata Outcome B_Wins Draw A_Wins)

(def winner
  (λ ((handA : Hand) (handB : Hand)) : Outcome
    (NatToOutcome (+ (HandToNat handA) (% (- 4 (HandToNat handB)) 3)))))

(@ (interaction (@list A B))
 (def rockPaperScissors
  (λ (wagerAmount)
    (@ A
     (block
      (def handA (inputHand "First player, pick your hand"))
      (assert! (canReach end (== (@dot end outcome) A_Wins)))
      (def salt (randomUInt256))
      (@ verifiably (def commitment (digest salt handA)))
      (publish! commitment)
      (deposit! (+ wagerAmount escrowAmount))))

    (@ B
     (block
      (assert! (canReach end (== (@dot end outcome) B_Wins)))
      (def handB (inputHand "Second player, pick your hand"))
      (publish! handB)
      (deposit! wagerAmount)))

    (@ A (publish! salt handA))
    (verify! commitment)
    (def outcome (winner handA handB))

    (switch outcome
      (A_Wins (withdraw! Alice (* 2 wagerAmount)))
      (B_Wins (withdraw! Bob (* 2 wagerAmount)))
      (Draw (withdraw! Alice wagerAmount)
            (withdraw! Bob wagerAmount)))

    outcome)))

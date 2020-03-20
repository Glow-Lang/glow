(defdata Hand Rock Paper Scissors)
(def NatToHand (λ ((x : int)) ;; Will be auto-generated soon
  (require! (<= 0 x)) (require! (< x 3))
  (if (= x 0) Rock (if (= x 1) Paper Scissors))))
(def HandToNat (λ ((x : Hand)) ;; Will be auto-generated soon
  (switch x (Rock 0) (Paper 1) (Scissors 2))))

(defdata Outcome B_Wins Draw A_Wins)
(def NatToOutcome (λ ((x : int)) ;; will be auto-generated soon
  (require! (and (<= 0 x) (< x 3)))
  (if (= x 0) B_Wins (if (= x 1) Draw A_Wins))))
(def OutcomeToNat (λ ((x : Outcome)) ;; Will be auto-generated soon
  (switch x (B_Wins 0) (Draw 1) (A_Wins 2))))

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
      (deposit! wagerAmount)))

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

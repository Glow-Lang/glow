(defdata Hand Rock Paper Scissors)
;; The below functions will be auto-generated soon
(def inputHand (λ (tag) (def x : Hand (input Hand tag)) x))
(def NatToHand (λ ((x : int)) (require! (and (<= 0 x) (< x 3))) (if (= x 0) Rock (if (= x 1) Paper Scissors))))
(def HandToNat (λ ((x : Hand)) (switch x (Rock 0) (Paper 1) (Scissors 2))))

(defdata Outcome B_Wins Draw A_Wins)
;; The below functions will be auto-generated soon
(def inputOutcome (λ (tag) (def x : Outcome (input Outcome tag)) x))
(def NatToOutcome (λ ((x : int)) (require! (and (<= 0 x) (< x 3))) (if (= x 0) B_Wins (if (= x 1) Draw A_Wins))))
(def OutcomeToNat (λ ((x : Outcome)) (switch x (B_Wins 0) (Draw 1) (A_Wins 2))))

(def winner
  (λ ((handA : Hand) (handB : Hand)) : Outcome
    (NatToOutcome (+ (HandToNat handA) (mod (- 4 (HandToNat handB)) 3)))))

(@ (interaction (@list A B))
 (def rockPaperScissors
  (λ (wagerAmount)
    (@ A (def handA (inputHand "First player, pick your hand")))
    ;;(@ A (assert! (canReach end (== (@dot end outcome) A_Wins))))
    (@ A (def salt (randomUInt256)))
    (@ A (@ verifiably (def commitment (digest salt handA))))
    (@ A (publish! commitment))
    (@ A (deposit! wagerAmount))

    ;;(@ B (assert! (canReach end (== (@dot end outcome) B_Wins))))
    (@ B (def handB (inputHand "Second player, pick your hand")))
    (@ B (publish! handB))
    (@ B (deposit! wagerAmount))

    (@ A (publish! salt handA))
    (verify! commitment)
    (def outcome (winner handA handB))

    ;; (label end)
    (switch outcome
      (A_Wins (withdraw! A (* 2 wagerAmount)))
      (B_Wins (withdraw! B (* 2 wagerAmount)))
      (Draw (withdraw! A wagerAmount)
            (withdraw! B wagerAmount)))

    outcome)))

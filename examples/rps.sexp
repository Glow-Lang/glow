(@module
(defdata Hand Rock Paper Scissors)
(defdata Outcome B_Wins Draw A_Wins)

(def winner
  (λ ((handA : Hand) (handB : Hand)) : Outcome
    (@app (@dot Outcome ofNat) (mod (+ (@app (@dot Hand toNat) handA) (- 4 (@app (@dot Hand toNat) handB))) 3))))

(@ (interaction (@list A B))
 (def rockPaperScissors
  (λ (wagerAmount)
    (@ A (def handA (@app (@dot Hand input) "First player, pick your hand")))
    ;;(@ A (assert! (@app canReach end (== (@dot end outcome) A_Wins))))
    (@ A (def salt (@app randomUInt256)))
    (@ A (@ verifiably (def commitment (digest (@tuple salt handA)))))
    (publish! A commitment)
    (deposit! A wagerAmount)

    ;;(@ B (assert! (@app canReach end (== (@dot end outcome) B_Wins))))
    (@ B (def handB (@app (@dot Hand input) "Second player, pick your hand")))
    (publish! B handB)
    (deposit! B wagerAmount)

    (publish! A salt handA)
    (verify! commitment)
    (def outcome (@app winner handA handB))

    ;; (label end)
    (switch outcome
      (A_Wins (withdraw! A (* 2 wagerAmount)))
      (B_Wins (withdraw! B (* 2 wagerAmount)))
      (Draw (withdraw! A wagerAmount)
            (withdraw! B wagerAmount)))

    outcome))))

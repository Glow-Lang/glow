(@module
(@debug-label dlb)
(defdata Hand Rock Paper Scissors)
(@debug-label dlb0)
(defdata Outcome B_Wins Draw A_Wins)

(@debug-label dlb1)
(def winner
  (λ ((handA : Hand) (handB : Hand)) : Outcome
    (@debug-label dlb2)
    (@app (@dot Outcome ofNat) (mod (+ (@app (@dot Hand toNat) handA) (- 4 (@app (@dot Hand toNat) handB))) 3))))

(@debug-label dlb3)
(@ (interaction (@list A B))
 (def rockPaperScissors
  (λ (wagerAmount)
    (@debug-label dlb4)
    (@ A (def handA (@app (@dot Hand input) "First player, pick your hand")))
    ;;(@ A (assert! (@app canReach end (== (@dot end outcome) A_Wins))))
    (@debug-label dlb5)
    (@ A (def salt (@app randomUInt256)))
    (@debug-label dlb6)
    (@ (verifiably! A) (def commitment (digest (@tuple salt handA))))
    (@debug-label dlb7)
    (publish! A commitment)
    (@debug-label dlb8)
    (deposit! A wagerAmount)

    ;;(@ B (assert! (@app canReach end (== (@dot end outcome) B_Wins))))
    (@debug-label dlb9)
    (@ B (def handB (@app (@dot Hand input) "Second player, pick your hand")))
    (@debug-label dlb10)
    (publish! B handB)
    (@debug-label dlb11)
    (deposit! B wagerAmount)

    (@debug-label dlb12)
    (publish! A salt handA)
    (@debug-label dlb13)
    (verify! commitment)
    (@debug-label dlb14)
    (def outcome (@app winner handA handB))

    ;; (label end)
    (@debug-label dlb15)
    (switch outcome
      (A_Wins (@debug-label dlb16)
              (withdraw! A (* 2 wagerAmount)))
      (B_Wins (@debug-label dlb17)
              (withdraw! B (* 2 wagerAmount)))
      (Draw (@debug-label dlb18)
            (withdraw! A wagerAmount)
            (@debug-label dlb19)
            (withdraw! B wagerAmount)))

    (@debug-label dlb20)
    outcome))))

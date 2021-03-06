(@module
(defdata Hand Rock Paper Scissors)
(defdata Outcome B_Wins Draw A_Wins)

(def winner
  (λ ((handA : Hand) (handB : Hand)) : Outcome
    ((@dot Outcome ofNat) (mod (+ ((@dot Hand toNat) handA) (- 4 ((@dot Hand toNat) handB))) 3))))

(@ (interaction (@list A B))
 (def rockPaperScissors
  (λ (wagerAmount)
    (@ A
       (splice
         (def handA ((@dot Hand input) "First player, pick your hand"))
         (def salt (randomUInt256))))
    (@ (verifiably! A) (def commitment (digest (@tuple salt handA))))
    (publish! A commitment)
    (deposit! A wagerAmount)

    (@ B (def handB ((@dot Hand input) "Second player, pick your hand")))
    (publish! B handB)
    (deposit! B wagerAmount)

    (publish! A salt handA)
    (verify! commitment)
    (def outcome (winner handA handB))

    ;; (label end)
    (switch outcome
      (A_Wins (withdraw! A (* 2 wagerAmount)))
      (B_Wins (withdraw! B (* 2 wagerAmount)))
      (Draw (withdraw! A wagerAmount)
            (withdraw! B wagerAmount)))

    outcome))))

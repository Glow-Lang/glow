(defdata Hand
         Rock
         Paper
         Scissors
         with:
         (block (def tmp (λ (tag1) (def x5 : name (input name tag1)) x5))
                (def tmp0 (λ ((x6 : name)) (switch x6 (Rock 0) (Paper 1) (Scissors 2))))
                (def tmp1 (λ ((x7 : name)) (switch x7 (0 Rock) (1 Paper) (2 Scissors))))
                (@record (input tmp) (toNat tmp0) (ofNat tmp1))))
(def inputHand (λ (tag) (def x : Hand (input Hand tag)) x))
(def NatToHand
     (λ ((x0 : int))
        (def tmp2 (@app <= 0 x0))
        (def tmp3 (and tmp2 (@app < x0 3)))
        (require! tmp3)
        (def tmp4 (@app = x0 0))
        (if tmp4 Rock (block (def tmp5 (@app = x0 1)) (if tmp5 Paper Scissors)))))
(def HandToNat (λ ((x1 : Hand)) (switch x1 (Rock 0) (Paper 1) (Scissors 2))))
(defdata Outcome
         B_Wins
         Draw
         A_Wins
         with:
         (block (def tmp6 (λ (tag2) (def x8 : name (input name tag2)) x8))
                (def tmp7 (λ ((x9 : name)) (switch x9 (B_Wins 0) (Draw 1) (A_Wins 2))))
                (def tmp8 (λ ((x10 : name)) (switch x10 (0 B_Wins) (1 Draw) (2 A_Wins))))
                (@record (input tmp6) (toNat tmp7) (ofNat tmp8))))
(def inputOutcome (λ (tag0) (def x2 : Outcome (input Outcome tag0)) x2))
(def NatToOutcome
     (λ ((x3 : int))
        (def tmp9 (@app <= 0 x3))
        (def tmp10 (and tmp9 (@app < x3 3)))
        (require! tmp10)
        (def tmp11 (@app = x3 0))
        (if tmp11 B_Wins (block (def tmp12 (@app = x3 1)) (if tmp12 Draw A_Wins)))))
(def OutcomeToNat (λ ((x4 : Outcome)) (switch x4 (B_Wins 0) (Draw 1) (A_Wins 2))))
(def winner
     (λ ((handA : Hand) (handB : Hand))
        :
        Outcome
        (def tmp13 (@app HandToNat handA))
        (def tmp14 (@app HandToNat handB))
        (def tmp15 (@app - 4 tmp14))
        (def tmp16 (@app mod tmp15 3))
        (def tmp17 (@app + tmp13 tmp16))
        (@app NatToOutcome tmp17)))
(@interaction
 ((@list A B))
 (def rockPaperScissors
      (λ (wagerAmount)
         (@ A (def handA0 (@app inputHand "First player, pick your hand")))
         (@ A (def salt (@app randomUInt256)))
         (@ A (def tmp18 (@tuple salt handA0)))
         (@ A (def commitment (digest tmp18)))
         (@ A (publish! commitment))
         (@ A (deposit! wagerAmount))
         (@ B (def handB0 (@app inputHand "Second player, pick your hand")))
         (@ B (publish! handB0))
         (@ B (deposit! wagerAmount))
         (@ A (publish! salt handA0))
         (def tmp19 (@tuple salt handA0))
         (def tmp20 (digest tmp19))
         (def tmp21 (@app = commitment tmp20))
         (require! tmp21)
         (def outcome (@app winner handA0 handB0))
         (switch outcome
                 (A_Wins (def tmp22 (@app * 2 wagerAmount)) (withdraw! A tmp22))
                 (B_Wins (def tmp23 (@app * 2 wagerAmount)) (withdraw! B tmp23))
                 (Draw (withdraw! A wagerAmount) (withdraw! B wagerAmount)))
         outcome)))

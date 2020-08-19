(@module
(defdata Hand
         Rock Paper Scissors)

(def tmp
               (λ (tag)
                  (: Hand)
                  (def x (input Hand tag))
                  (ann x Hand)
                  (return x)))
(def tmp0
               (λ ((x0 : Hand))
                  (: nat)
                  (switch x0
                          ((@app-ctor Rock) (return 0))
                          ((@app-ctor Paper) (return 1))
                          ((@app-ctor Scissors) (return 2)))))
(def tmp1
               (λ ((x1 : nat))
                  (: Hand)
                  (switch x1
                          (0 (return Rock))
                          (1 (return Paper))
                          (2 (return Scissors)))))
(def Hand1 (@record (input tmp) (toNat tmp0) (ofNat tmp1)))

(defdata Outcome
         B_Wins Draw A_Wins)

(def tmp2
               (λ (tag0)
                  (: Outcome)
                  (def x2 (input Outcome tag0))
                  (ann x2 Outcome)
                  (return x2)))
(def tmp3
               (λ ((x3 : Outcome))
                  (: nat)
                  (switch x3
                          ((@app-ctor B_Wins) (return 0))
                          ((@app-ctor Draw) (return 1))
                          ((@app-ctor A_Wins) (return 2)))))
(def tmp4
               (λ ((x4 : nat))
                  (: Outcome)
                  (switch x4
                          (0 (return B_Wins))
                          (1 (return Draw))
                          (2 (return A_Wins)))))
(def Outcome1 (@record (input tmp2) (toNat tmp3) (ofNat tmp4)))

(def winner
     (λ ((handA : Hand) (handB : Hand))
        (: Outcome)
        (def tmp5 (@dot Outcome1 ofNat))
        (def tmp6 (@dot Hand1 toNat))
        (def tmp7 (@app tmp6 handA))
        (def tmp8 (@dot Hand1 toNat))
        (def tmp9 (@app tmp8 handB))
        (def tmp10 (@app - 4 tmp9))
        (def tmp11 (@app + tmp7 tmp10))
        (def tmp12 (@app mod tmp11 3))
        (return (@app tmp5 tmp12))))
(def rockPaperScissors
     (@make-interaction
      ((@list A B))
      (wagerAmount)
      ()
      (@ A (def tmp13 (@dot Hand1 input)))
      (@ A (def handA0 (@app tmp13 "First player, pick your hand")))
      (@ A (def salt (@app randomUInt256)))
      (@ A (def tmp14 (@tuple salt handA0)))
      (@ A (def commitment (digest tmp14)))
      (publish! A commitment)
      (deposit! A wagerAmount)
      (@ B (def tmp15 (@dot Hand1 input)))
      (@ B (def handB0 (@app tmp15 "Second player, pick your hand")))
      (publish! B handB0)
      (deposit! B wagerAmount)
      (publish! A salt)
      (publish! A handA0)
      (def tmp16 (@tuple salt handA0))
      (def tmp17 (digest tmp16))
      (def tmp18 (== commitment tmp17))
      (require! tmp18)
      (def outcome (@app winner handA0 handB0))
      (switch outcome
              ((@app-ctor A_Wins) (def tmp19 (@app * 2 wagerAmount)) (withdraw! A tmp19))
              ((@app-ctor B_Wins) (def tmp20 (@app * 2 wagerAmount)) (withdraw! B tmp20))
              ((@app-ctor Draw) (withdraw! A wagerAmount) (withdraw! B wagerAmount)))
      (return outcome)))
(return (@tuple)))

(hash (* (type:arrow [(type:name 'Int) (type:name 'Int)] (type:name 'Int)))
      (+ (type:arrow [(type:name 'Int) (type:name 'Int)] (type:name 'Int)))
      (- (type:arrow [(type:name 'Int) (type:name 'Int)] (type:name 'Int)))
      (A (type:name 'Participant))
      (A_Wins (type:name 'Outcome0))
      (B (type:name 'Participant))
      (B_Wins (type:name 'Outcome0))
      (Draw (type:name 'Outcome0))
      (Hand1 (type:record
              (symdict ('toNat
                        (type:arrow
                         [(type:name 'Hand0)]
                         (type:name-subtype 'Nat (type:name 'Int))))
                       ('ofNat
                        (type:arrow
                         [(type:name-subtype 'Nat (type:name 'Int))]
                         (type:name 'Hand0)))
                       ('input
                        (type:arrow
                         [(type:name 'Bytes)]
                         (type:name 'Hand0))))))
      (Outcome1
       (type:record
        (symdict ('toNat
                  (type:arrow
                   [(type:name 'Outcome0)]
                   (type:name-subtype 'Nat (type:name 'Int))))
                 ('ofNat
                  (type:arrow
                   [(type:name-subtype 'Nat (type:name 'Int))]
                   (type:name 'Outcome0)))
                 ('input
                  (type:arrow [(type:name 'Bytes)] (type:name 'Outcome0))))))
      (Paper (type:name 'Hand0))
      (Rock (type:name 'Hand0))
      (Scissors (type:name 'Hand0))
      (commitment (type:name 'Digest))
      (handA (type:name 'Hand0))
      (handA0 (type:name 'Hand0))
      (handB (type:name 'Hand0))
      (handB0 (type:name 'Hand0))
      (mod (type:arrow
            [(type:name 'Int) (type:name-subtype 'Nat (type:name 'Int))]
            (type:name-subtype 'Nat (type:name 'Int))))
      (outcome (type:name 'Outcome0))
      (randomUInt256 (type:arrow [] (type:name-subtype 'Nat (type:name 'Int))))
      (rockPaperScissors
       (type:arrow
        [(type:name 'Participant) (type:name 'Participant) (type:name 'Int)]
        (type:name 'Outcome0)))
      (salt (type:name-subtype 'Nat (type:name 'Int)))
      (tag (type:name 'Bytes))
      (tag0 (type:name 'Bytes))
      (tmp (type:arrow [(type:name 'Bytes)] (type:name 'Hand0)))
      (tmp0 (type:arrow
             [(type:name 'Hand0)]
             (type:name-subtype 'Nat (type:name 'Int))))
      (tmp1 (type:arrow
             [(type:name-subtype 'Nat (type:name 'Int))]
             (type:name 'Hand0)))
      (tmp10 (type:name 'Int))
      (tmp11 (type:name 'Int))
      (tmp12 (type:name-subtype 'Nat (type:name 'Int)))
      (tmp13 (type:arrow [(type:name 'Bytes)] (type:name 'Hand0)))
      (tmp14 (type:tuple
              [(type:name-subtype 'Nat (type:name 'Int)) (type:name 'Hand0)]))
      (tmp15 (type:arrow [(type:name 'Bytes)] (type:name 'Hand0)))
      (tmp16 (type:tuple
              [(type:name-subtype 'Nat (type:name 'Int)) (type:name 'Hand0)]))
      (tmp17 (type:name 'Digest))
      (tmp18 (type:name 'Bool))
      (tmp19 (type:name 'Int))
      (tmp2 (type:arrow [(type:name 'Bytes)] (type:name 'Outcome0)))
      (tmp20 (type:name 'Int))
      (tmp3 (type:arrow
             [(type:name 'Outcome0)]
             (type:name-subtype 'Nat (type:name 'Int))))
      (tmp4 (type:arrow
             [(type:name-subtype 'Nat (type:name 'Int))]
             (type:name 'Outcome0)))
      (tmp5 (type:arrow
             [(type:name-subtype 'Nat (type:name 'Int))]
             (type:name 'Outcome0)))
      (tmp6 (type:arrow
             [(type:name 'Hand0)]
             (type:name-subtype 'Nat (type:name 'Int))))
      (tmp7 (type:name-subtype 'Nat (type:name 'Int)))
      (tmp8 (type:arrow
             [(type:name 'Hand0)]
             (type:name-subtype 'Nat (type:name 'Int))))
      (tmp9 (type:name-subtype 'Nat (type:name 'Int)))
      (wagerAmount (type:name 'Int))
      (winner (type:arrow
               [(type:name 'Hand0) (type:name 'Hand0)]
               (type:name 'Outcome0)))
      (x (type:name 'Hand0))
      (x0 (type:name-subtype 'Nat (type:name 'Int)))
      (x1 (type:name 'Hand0))
      (x2 (type:name 'Outcome0))
      (x3 (type:name-subtype 'Nat (type:name 'Int)))
      (x4 (type:name 'Outcome0))
      (0 (type:name-subtype 'Nat (type:name 'Int)))
      (1 (type:name-subtype 'Nat (type:name 'Int)))
      (2 (type:name-subtype 'Nat (type:name 'Int)))
      (3 (type:name-subtype 'Nat (type:name 'Int)))
      (4 (type:name-subtype 'Nat (type:name 'Int)))
      ("First player, pick your hand" (type:name 'Bytes))
      ("Second player, pick your hand" (type:name 'Bytes))
      ((@app * 2 wagerAmount) (type:name 'Int))
      ((@app randomUInt256) (type:name-subtype 'Nat (type:name 'Int)))
      ((@app winner handA0 handB0) (type:name 'Outcome0))
      ((@dot Hand1 input) (type:arrow [(type:name 'Bytes)] (type:name 'Hand0)))
      ((@dot Hand1 toNat)
       (type:arrow
        [(type:name 'Hand0)]
        (type:name-subtype 'Nat (type:name 'Int))))
      ((@dot Outcome1 ofNat)
       (type:arrow
        [(type:name-subtype 'Nat (type:name 'Int))]
        (type:name 'Outcome0)))
      ((@tuple salt handA0)
       (type:tuple
        [(type:name-subtype 'Nat (type:name 'Int)) (type:name 'Hand0)]))
      ((input Hand tag) (type:name 'Hand0))
      ((input Outcome tag0) (type:name 'Outcome0)))

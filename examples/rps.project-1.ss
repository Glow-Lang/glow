#|
To generate from the command-line in the glow directory:
./unit-tests.ss pass project-1 examples/rps.sexp
To run from gxi in the glow directory, assuming gerbil-etherum is in a sibling directory:
> (add-load-path (path-normalize "../gerbil-ethereum"))
> (import "examples/rps.project-1.ss" :mukn/ethereum/t/signing-test :clan/persist/content-addressing)
> ((rockPaperScissors alice-address bob-address) 1)
In thread A:
*** ERROR IN std/misc/hash#hash-ensure-ref, -515899390@10.31 -- No such slot #<poo #39> sexp
0  std/misc/hash#hash-ensure-ref -515899390@10:31  (_default834_)
        _table832_ == '#<table #40>
        _key833_ == 'sexp
1  mukn/glow/compiler/project/runtime-1#input
2  mukn/glow/examples/rps.project-1#tmp "examples/rps.project-1.ss"@27:26
        #:x2787 = #!unbound
        #:tag2785 = "First player, pick your hand"
3  mukn/glow/examples/rps.project-1#tmp "examples/rps.project-1.ss"@27:10
        #:tag2785 = "First player, pick your hand"
*** ERROR IN ##thread-deadlock-action! -- Deadlock detected
|#
(export #t)
(import :mukn/glow/compiler/project/runtime-1)
(define-type Hand (Sum Rock: (Tuple) Paper: (Tuple) Scissors: (Tuple)))
(define-sum-constructors Hand Rock Paper Scissors)
(def Rock (Hand-Rock (Tuple)))
(def Paper (Hand-Paper (Tuple)))
(def Scissors (Hand-Scissors (Tuple)))
(def tmp (λ (tag) (def x (input Hand1 tag)) x))
(def tmp0
     (λ (x0)
        (match x0
               ((Hand-Rock (vector)) 0)
               ((Hand-Paper (vector)) 1)
               ((Hand-Scissors (vector)) 2))))
(def tmp1 (λ (x1) (match x1 (0 Rock) (1 Paper) (2 Scissors))))
(def Hand1 (.o (input tmp) (toNat tmp0) (ofNat tmp1)))
(define-type Outcome (Sum B_Wins: (Tuple) Draw: (Tuple) A_Wins: (Tuple)))
(define-sum-constructors Outcome B_Wins Draw A_Wins)
(def B_Wins (Outcome-B_Wins (Tuple)))
(def Draw (Outcome-Draw (Tuple)))
(def A_Wins (Outcome-A_Wins (Tuple)))
(def tmp2 (λ (tag0) (def x2 (input Outcome1 tag0)) x2))
(def tmp3
     (λ (x3)
        (match x3
               ((Outcome-B_Wins (vector)) 0)
               ((Outcome-Draw (vector)) 1)
               ((Outcome-A_Wins (vector)) 2))))
(def tmp4 (λ (x4) (match x4 (0 B_Wins) (1 Draw) (2 A_Wins))))
(def Outcome1 (.o (input tmp2) (toNat tmp3) (ofNat tmp4)))
(def winner
     (λ (handA handB)
        (def tmp5 (.@ Outcome1 ofNat))
        (def tmp6 (.@ Hand1 toNat))
        (def tmp7 (%%app tmp6 handA))
        (def tmp8 (.@ Hand1 toNat))
        (def tmp9 (%%app tmp8 handB))
        (def tmp10 (%%app - 4 tmp9))
        (def tmp11 (%%app + tmp7 tmp10))
        (def tmp12 (%%app mod tmp11 3))
        (%%app tmp5 tmp12)))
(def rockPaperScissors-consensus
     (lambda (in out A B)
       (lambda (wagerAmount)
         (parameterize ((current-input-channel in)
                        (current-output-channel out))
           (begin0 (let ()
                     (consensus:set-participant A)
                     (consensus:set-participant A)
                     (consensus:set-participant A)
                     (consensus:set-participant A)
                     (consensus:set-participant A)
                     (consensus:set-participant A)
                     (def commitment (expect-published 'commitment))
                     (consensus:set-participant A)
                     (expect-deposited wagerAmount)
                     (consensus:set-participant B)
                     (consensus:set-participant B)
                     (consensus:set-participant B)
                     (def handB0 (expect-published 'handB0))
                     (consensus:set-participant B)
                     (expect-deposited wagerAmount)
                     (consensus:set-participant A)
                     (def salt (expect-published 'salt))
                     (consensus:set-participant A)
                     (def handA0 (expect-published 'handA0))
                     (def tmp16 (vector salt handA0))
                     (def tmp17
                          (digest (@list (cons (Tuple #f Hand1) tmp16))))
                     (def tmp18 (== commitment tmp17))
                     (assert! tmp18)
                     (def outcome (%%app winner handA0 handB0))
                     (match outcome
                            ((Outcome-A_Wins (vector))
                             (def tmp19 (%%app * 2 wagerAmount))
                             (expect-withdrawn A tmp19))
                            ((Outcome-B_Wins (vector))
                             (def tmp20 (%%app * 2 wagerAmount))
                             (expect-withdrawn B tmp20))
                            ((Outcome-Draw (vector))
                             (expect-withdrawn A wagerAmount)
                             (expect-withdrawn B wagerAmount)))
                     outcome)
                   (consensus:end-interaction))))))
(def rockPaperScissors-A
     (lambda (in0 out0 A B)
       (lambda (wagerAmount)
         (parameterize ((current-input-channel in0)
                        (current-output-channel out0))
           (begin0 (let ()
                     (participant:set-participant A)
                     (def tmp13 (.@ Hand1 input))
                     (participant:set-participant A)
                     (def handA0
                          (%%app tmp13 "First player, pick your hand"))
                     (participant:set-participant A)
                     (def salt (%%app randomUInt256))
                     (participant:set-participant A)
                     (def tmp14 (vector salt handA0))
                     (participant:set-participant A)
                     (def commitment
                          (digest (@list (cons (Tuple #f Hand1) tmp14))))
                     (participant:set-participant A)
                     (add-to-publish 'commitment commitment)
                     (participant:set-participant A)
                     (add-to-deposit wagerAmount)
                     (participant:set-participant B)
                     (participant:set-participant B)
                     (participant:set-participant B)
                     (def handB0 (expect-published 'handB0))
                     (participant:set-participant B)
                     (expect-deposited wagerAmount)
                     (participant:set-participant A)
                     (add-to-publish 'salt salt)
                     (participant:set-participant A)
                     (add-to-publish 'handA0 handA0)
                     (def tmp16 (vector salt handA0))
                     (def tmp17
                          (digest (@list (cons (Tuple #f Hand1) tmp16))))
                     (def tmp18 (== commitment tmp17))
                     (assert! tmp18)
                     (def outcome (%%app winner handA0 handB0))
                     (match outcome
                            ((Outcome-A_Wins (vector))
                             (def tmp19 (%%app * 2 wagerAmount))
                             (add-to-withdraw A tmp19))
                            ((Outcome-B_Wins (vector))
                             (def tmp20 (%%app * 2 wagerAmount))
                             (expect-withdrawn B tmp20))
                            ((Outcome-Draw (vector))
                             (add-to-withdraw A wagerAmount)
                             (expect-withdrawn B wagerAmount)))
                     outcome)
                   (participant:end-interaction))))))
(def rockPaperScissors-B
     (lambda (in1 out1 A B)
       (lambda (wagerAmount)
         (parameterize ((current-input-channel in1)
                        (current-output-channel out1))
           (begin0 (let ()
                     (participant:set-participant A)
                     (participant:set-participant A)
                     (participant:set-participant A)
                     (participant:set-participant A)
                     (participant:set-participant A)
                     (participant:set-participant A)
                     (def commitment (expect-published 'commitment))
                     (participant:set-participant A)
                     (expect-deposited wagerAmount)
                     (participant:set-participant B)
                     (def tmp15 (.@ Hand1 input))
                     (participant:set-participant B)
                     (def handB0
                          (%%app tmp15 "Second player, pick your hand"))
                     (participant:set-participant B)
                     (add-to-publish 'handB0 handB0)
                     (participant:set-participant B)
                     (add-to-deposit wagerAmount)
                     (participant:set-participant A)
                     (def salt (expect-published 'salt))
                     (participant:set-participant A)
                     (def handA0 (expect-published 'handA0))
                     (def tmp16 (vector salt handA0))
                     (def tmp17
                          (digest (@list (cons (Tuple #f Hand1) tmp16))))
                     (def tmp18 (== commitment tmp17))
                     (assert! tmp18)
                     (def outcome (%%app winner handA0 handB0))
                     (match outcome
                            ((Outcome-A_Wins (vector))
                             (def tmp19 (%%app * 2 wagerAmount))
                             (expect-withdrawn A tmp19))
                            ((Outcome-B_Wins (vector))
                             (def tmp20 (%%app * 2 wagerAmount))
                             (add-to-withdraw B tmp20))
                            ((Outcome-Draw (vector))
                             (expect-withdrawn A wagerAmount)
                             (add-to-withdraw B wagerAmount)))
                     outcome)
                   (participant:end-interaction))))))
(def ((rockPaperScissors A B) wagerAmount)
     (def consensus->A (make-channel #f))
     (def consensus->B (make-channel #f))
     (def consensus->participants (@list consensus->A consensus->B))
     (def participant->consensus (make-channel #f))
     (def consensus-thread
          (spawn/name/params
           'consensus
           (lambda ()
             (parameterize ((current-address #f))
               ((rockPaperScissors-consensus
                 participant->consensus
                 consensus->participants
                 A
                 B)
                wagerAmount)))))
     (def participant-threads
          (@list (spawn/name/params
                  'A
                  (lambda ()
                    (parameterize ((current-address A))
                      ((rockPaperScissors-A
                        consensus->A
                        participant->consensus
                        A
                        B)
                       wagerAmount))))
                 (spawn/name/params
                  'B
                  (lambda ()
                    (parameterize ((current-address B))
                      ((rockPaperScissors-B
                        consensus->B
                        participant->consensus
                        A
                        B)
                       wagerAmount))))))
     (for-each thread-join! (cons consensus-thread participant-threads))
     (channel-close consensus->A)
     (channel-close consensus->B)
     (channel-close participant->consensus)
     'done)
(vector)

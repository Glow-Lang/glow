;;-*- Gerbil -*-
;; Right after contract projection

;; TODO: make all that into tests!

;; TODO: the compiler would finish with those steps
;(def contract-code-from-bepp xxx)
;(def contract-code
; (simple-contract-init
;   init-digest ;; TODO: the compiler would compute that from the initial state!!!
;   (assemble (append simple-contract-prelude simple-logging contract-code-from-bepp))))

(defrule (def-frame-variable name size)
  (begin
    (def frame-address (post-increment! framepointer@ size))
    (def name (&mloadat size frame-address))))

(defrule (def-frame (name (variable size) ...) body ...)
  (begin ;; TODO: reset the framepointer@
    (def-frame-variable variable size) ...
    (&begin
     (jumplabel 'name)
     body ...)))

(def contract-code-from-bepp

  ;;(def payForSignature

  ;;(@make-interaction
  ;;((@list Buyer Seller))
  ;; ((digest0 : Digest) (price : nat)) ==> used to generate frame when encoding initial state,
  ;; and when having the assumptions about state at given label.

  (def-frame (begin0 saved: ((Seller 20) (Buyer 20) (digest0 32) (price 32)) ;; size 12 for price?
                     published: ()
                     generated: ())
    ;; TODO: check that the user is an allowed user!
    ;; This is a Seller segment, so check that CALLER == Seller
    ;; (alternatively, allow for posting market and check that the message was signed by seller,
    ;; independently from who posted the thing...)

    ;; AKSHULLY, FOR A SIMPLE CONTRACT, WE DON'T NEED TO GENERATE CODE AT ALL FOR begin0,
    ;; BECAUSE Buyer WILL CREATE THE CONTRACT DIRECTLY IN THE PROPER STATE,
    ;; WITH A DIGEST FOR (cp0 Seller price digest0) AND MONEY PRE-DEPOSITED.
    ;; ---- OTHERWISE Seller WILL REFUSE TO PARTAKE.
    ;;
    ;; SO---DO SOME TREE-SHAKING AND ONLY GENERATE CODE FOR REACHABLE FRAMES.
    ;; FOR v0.1, it's OK to generate extra code!
    Buyer &check-participant!
    price &deposit!

    ;; Let's reserve at the end of the current frame some space to make sure we can build the next frame.
    ;; But build the frame by:
    ;; load everything that is not preserved at the same address, in reverse write order.
    ;; Write everything on top of the old frame, in memory, in left to right order.
    ;; When writing, determine whether it's OK to put in extra blanks or not,
    ;; depending on whether the next element is invariant between frames or not.
    (tail-call-frame cp0 (Seller 20) (price 32) (digest0 32)))

    ;; the tail-call-frame expands to:
    ;; loading in reverse write order: cp0 price digest0
    (&mloadat digest0 32)
    (&mloadat price 32)
    'cp0
    ;; writing left to right
    (&mstoreat @frame 2) ;; there's Seller after, so no /overwrite-after variant
    (&mstoreat/overwrite-after (+ frame@ 22) 32) ;; nothing after so /overwrite-after, but at 32, no matter
    (&mstoreat/overwrite-after (+ frame@ 54) 32)
    ;; drop in the next frame size as argument to tail-call!
    98
    [&jump 'tail-call]


  (def-frame (cp0 saved: ((Seller 20) (price 32) (digest0 32))
                  published: ((Signature 65))
                  generated: ())
    ;; TODO: handle timeouts somehow???

    ;; IF we have a brk yet no heap preservation from previous frames, update it now to be past generated.
    (+ frame@ (current-frame-size)) (&mstoreat brk@) ;; WE DON'T EVEN NEED A brk FOR v0.1

    (&check-participant-or-timeout! must-act: Seller or-end-in-favor-of: Buyer)

    ;; TODO: we need to amend our read and storage for more than 32-bytes
    Signature 65 &read-published-data-to-mem
    ;; TODO: check signature by calling "precompiled contract" #1 ECREC
    Seller (&mloadat 20) digest0 (&mloadat 32) Signature &isValidSignature &require!

    price (&mloadat 32) Seller (&mloadat 20) &withdraw!

    &end-contract!)

;;-*- Gerbil -*-
;; Last point Before Blockchain-End-Point-Projection

;;(defdata Action
;;  ((Bid TokenAmount) ;; uint96, token amount in wei
;;   (Close)))
;; replaced by (type+ (type* uint96) unit)
;; Its with: clause is expanded away as follows:
(def Bid (: (uint96 . -> . (type+ (type* uint96) unit)))
  (lambda (n) (sum_constructor 0 n)))
(def Close (: (type+ (type* uint96) unit))
  (sum_constructor 1 unit))
(def Action ()
  (record (show ...) (toString ...) ...))


;; NB: Since we know *BEFORE BEPP* that it's a top-level interaction,
;; We can do the contract creation and the first two steps in a single actual blockchain transaction,
;; and not have to generate timeouts or intermediate states.

(def simpleAuction
  (@make-interaction
   (participant-checkpoint
    pc ;; point B
    (state: (product ;; These are the live variables (published, for the contract)
             (Seller : uint160)
             (goods : uint128) ;; units of the ERC20 "FOO"
             (expirationTime : uint32)
             (escrow : uint96))) ;; Seller escrow in wei
     balances: ()
     escrows: ()
     next-participant: Seller
     session-type-of-the-body:
     ((body : () -> pc0)
      (timeout : () -> end)) ;; do we even generate a timeout for the first state????
     body:
     (for-any-participant
      (def tmp (FOO__is_positive goods))
      (require! tmp)
      (def tmp0 (currentTime))
      (def tmp1 (> expirationTime tmp0))
      (require! tmp1)
      (@continue-interaction pc0
       (Seller : uint160)
       (goods : uint128) ;; units of the ERC20 "FOO"
       (expirationTime : uint32)
       (escrow : uint96)))) ;; Seller escrow in wei

   (participant-checkpoint
    pc0
    (state: (product ;; These are the live variables (published, for the contract)
             (Seller : uint160)
             (goods : uint128) ;; units of the ERC20 "FOO"
             (expirationTime : uint32)
             (escrow : uint96))) ;; Seller escrow in wei
     balances: ()
     escrows: ()
     next-participant: Seller
     session-type-of-the-body:
     ((body : () -> auction)
      (timeout : () -> end)) ;; do we even generate a timeout for the first state????
     body:
     (for-participant (Seller)
      (@Seller FOO__deposit! goods) ;; FFI call?
      (@Seller wei__deposit! escrow)
      (@continue-interacton
       auction
       Seller Seller goods expirationTime escrow 0)))

   (participant-checkpoint
    auction
    (state: (product
             (Seller : uint160)
             (goods : uint128) ;; units of the ERC20 "FOO"
             (expirationTime : uint32)
             (escrow : uint96) ;; Seller escrow in wei
             (CurrentBidder : uint160)
             (currentBid : uint96))) ;; bid in wei
    ;; NB: if the way to access the balances and deadlines from the contract state
    ;; is uniform across all nodes of the interaction,
    ;; then all nodes can share a generic timeout handler
    ;; otherwise, we need to generate a new handler for each node
    balances: ((Seller wei: currentBid)
               (CurrentBidder FOO: goods))
    escrows: ((Seller wei: escrow))
    next-participant: #t
    session-type-of-the-body:
    ((body : () -> end)
     (timeout : () -> end))
    body:
    (choice
     (forall-participant (NewBidder)
      (@NewBidder (def tmp2 (: uint96) (input uint96 "Enter next bid")))
      (@NewBidder (def bid (: ...) (Bid tmp2)))
      (@NewBidder (publish! bid))
      (@NewBidder (wei__deposit! bid))
      (@NewBidder (assume! (> (@value goods) (@value bid)))) ;; should be ANF'ed
      (def tmp3 (currentTime))
      (def tmp4 (< tmp3 expirationTime))
      (require! tmp4)
      (def tmp5 (> bid currentBid))
      (require! tmp5)
      (wei__withdraw! CurrentBidder currentBid)
      (@continue-interaction
       auction
       Seller Bidder goods expirationTime escrow bid))
     (for-participant (Seller)
      (def tmp6 (currentTime))
      (def tmp7 (>= tmp6 expirationTime))
      (require! tmp7)
      (wei__withdraw! Seller currentBid)
      (FOO__withdraw! CurrentBidder goods)
      (@end-interaction))
     #| Implicitly added for timeouts:
     (for-participant (CurrentBidder) ;; in practice, CurrentBidder
      (def tmp8 (currentTime))
      (def tmp9 (+ expirationTime timeoutDelay)) ;; constant folded???
      (def tmp10 (>= tmp8 tmp9))
      (require! tmp10)
      (wei__withdraw! Seller currentBid)
      (FOO__withdraw! CurrentBidder goods)
      (@end-interaction))
     |#
))

   #|
   ;; implicit end state to every top-level interaction
   (participant-checkpoint
    end
    (state: top)
    balances: ()
    escrows: ()
    next-participant: #f
    session-type-of-the-body:
    ()
    body:))
    |#


(export #t)

(import
  :std/misc/list
  :clan/base :clan/pure/dict/assq
  :clan/poo/object :clan/poo/brace :clan/poo/io
  :clan/ethereum/types :clan/ethereum/assets)

;; The state for the "monad" of consensual computations, with two variants,
;; depending on whether the current participant is active or passive

(define-type BlockCtx
  {
   ;; Log a withdrawal. Takes the participant doing the withdrawal, the asset name,
   ;; and the amount.
   ;;
   ;; <- @ Address Symbol Nat
   .add-to-withdraw:
   ;; TODO: Shouldn't we be taking a Role rather than an Address as argument?
   ;; This would make both static verification and contract generation much simpler.
   ;; TODO: One the other hand, support multiple asset classes in a same transaction,
   ;; there again in a per-invocation static finite set.
   (λ (self address asset-sym amount)
     ;; TODO: make sure we check that the amounts are balanced at the end of the transaction
     (modify! (.@ self withdrawals)
              (cut assq-update <> asset-sym
                   (cut assq-update <> address (cut + <> amount) 0)
                   [])))

   .add-to-deposit: ;; <- @ Address Symbol Nat
   (λ (self address asset-sym amount)
     (modify! (.@ self deposits)
              (cut assq-update <> asset-sym (cut + <> amount) 0)))

   ;; Return an alist of the total withdrawals for each asset,
   ;; across all participants. Assets may be missing of no withdrawals
   ;; were made.
   ;;
   ;; Alist (Nat <- Symbol) <- @
   .total-withdrawals:
   (λ (self)
      (map
        (lambda (kv)
          (def asset-sym (car kv))
          (def addr-to-amount (cdr kv))
          (def total (apply + (map cdr addr-to-amount)))
          [asset-sym . total])
        (.@ self withdrawals)))
   })

(define-type PassiveBlockCtx
  (.+
    (Record
      deposits: [(List Any)] ;; Alist Nat <- Asset
      withdrawals: [(List Any)] ;; Alist (Alist Nat <- Address) <- Asset
      inbox: [Any]) ;; BytesInputPort
    {(:: @ BlockCtx)
    .make: (lambda (in-bytes) {deposits: []
                               withdrawals: []
                               inbox: (open-input-u8vector in-bytes)})
    .expect-published: ;; : t <- Symbol t:Type
    (λ (self _name type)
      ;; ignore name: by order not by name
      (unmarshal type (.@ self inbox)))}))

(define-type ActiveBlockCtx
  (.+
    (Record
      deposits: [(List Any)] ;; Alist Nat <- Asset
      withdrawals: [(List Any)] ;; Alist (Alist Nat <- Address) <- Asset
      outbox: [Any]) ;; BytesOutputPort
    {(:: @ BlockCtx)
    .make: (lambda () {deposits: []
                       withdrawals: []
                       outbox: (open-output-u8vector)})
    .add-to-published: ;; <- @ Symbol t:Type t
    (λ (self _name type value)
      ;; for debugging, accumulate name and type in a parallel data structure?
      (marshal type value (.@ self outbox)))}))

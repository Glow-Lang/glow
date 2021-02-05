(export #t)

(import
  :std/misc/list
  :clan/base :clan/pure/dict/assq
  :clan/poo/object :clan/poo/brace :clan/poo/io
  :mukn/ethereum/types)

;; The state for the "monad" of consensual computations, with two variants,
;; depending on whether the current participant is active or passive

;; add-to-withdraw : <- Address Nat
(define-type BlockCtx
  {.add-to-withdraw: ;; <- @ Address Nat
   ;; TODO: Shouldn't we be taking a Role rather than an Address as argument?
   ;; This would make both static verification and contract generation much simpler.
   ;; TODO: One the other hand, support multiple asset classes in a same transaction,
   ;; there again in a per-invocation static finite set.
   (λ (self address amount)
     ;; TODO: make sure we check that the amounts are balanced at the end of the transaction
     (modify! (.@ self withdrawals)
              (cut assq-update <> address (cut + <> amount) 0)))
   .add-to-deposit: ;; <- @ Nat
   (λ (self address amount)
     (modify! (.@ self deposits) (cut + <> amount)))
   })

(define-type PassiveBlockCtx
  (.+
    (Record
      deposits: [Nat]
      withdrawals: [(List Any)] ;; Alist Nat <- Address
      inbox: [Any]) ;; BytesInputPort
    {(:: @ BlockCtx)
    .make: (lambda (in-bytes) {deposits: 0
                               withdrawals: []
                               inbox: (open-input-u8vector in-bytes)})
    .expect-published: ;; : t <- Symbol t:Type
    (λ (self _name type)
      ;; ignore name: by order not by name
      (unmarshal type (.@ self inbox)))}))

(define-type ActiveBlockCtx
  (.+
    (Record
      deposits: [Nat]
      withdrawals: [(List Any)] ;; Alist Nat <- Address
      outbox: [Any]) ;; BytesOutputPort
    {(:: @ BlockCtx)
    .make: (lambda () {deposits: 0
                       withdrawals: []
                       outbox: (open-output-u8vector)})
    .add-to-published: ;; <- @ Symbol t:Type t
    (λ (self _name type value)
      ;; for debugging, accumulate name and type in a parallel data structure?
      (marshal type value (.@ self outbox)))}))

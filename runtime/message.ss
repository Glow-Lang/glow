(export #t)

(import
  :std/misc/list
  :clan/base :clan/pure/dict/assq
  :clan/poo/poo :clan/poo/brace :clan/poo/io
  :mukn/ethereum/types)

;; The state for the "monad" of consensual computations, with two variants,
;; depending on whether the current participant is active or passive

(defstruct $BlockCtx
  (deposits ;; : Nat
   withdrawals) ;; : (Alist Nat <- Address)
  transparent: #t)
(defstruct ($PassiveBlockCtx $BlockCtx)
  (inbox) ;; : BytesInputPort
  transparent: #t)
(defstruct ($ActiveBlockCtx $BlockCtx)
  (outbox) ;; : BytesOutputPort
  transparent: #t)

;; add-to-withdraw : <- Address Nat
(define-type BlockCtx
  {.add-to-withdraw: ;; <- @ Address Nat
   ;; TODO: Shouldn't we be taking a Role rather than an Address as argument?
   ;; This would make both static verification and contract generation much simpler.
   ;; TODO: One the other hand, support multiple asset classes in a same transaction,
   ;; there again in a per-invocation static finite set.
   (λ (self address amount)
     ;; TODO: make sure we check that the amounts are balanced at the end of the transaction
     (modify! ($BlockCtx-withdrawals self)
              (cut assq-update <> address (cut + <> amount) 0)))
   .add-to-deposit: ;; <- @ Nat
   (λ (self address amount)
     (modify! ($BlockCtx-deposits self) (cut + <> amount)))
   })

(define-type PassiveBlockCtx
  {(:: @ BlockCtx)
   .element?: $PassiveBlockCtx?
   .make: (lambda (in-bytes) ($PassiveBlockCtx 0 [] (open-input-u8vector in-bytes)))
   .expect-published: ;; : t <- Symbol t:Type
   (λ (self _name type)
     ;; ignore name: by order not by name
     (unmarshal type ($PassiveBlockCtx-inbox self)))})

(define-type ActiveBlockCtx
  {(:: @ BlockCtx)
   .element?: $ActiveBlockCtx?
   .make: (lambda () ($ActiveBlockCtx 0 [] (open-output-u8vector)))
   .add-to-published: ;; <- @ Symbol t:Type t
   (λ (self _name type value)
     ;; for debugging, accumulate name and type in a parallel data structure?
     (marshal type value ($ActiveBlockCtx-outbox self)))})

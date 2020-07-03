(export #t)

(import
  :gerbil/gambit/threads
  :std/error :std/misc/completion :std/text/hex
  :clan/utils/base :clan/utils/concurrency :clan/utils/failure :clan/utils/option :clan/utils/maybe
  :clan/net/json-rpc
  :clan/poo/poo :clan/poo/brace :clan/poo/io
  (only-in :clan/poo/mop define-type Fun)
  (only-in :clan/poo/type Maybe)
  :clan/runtime/db :clan/runtime/persist
  ./types ./ethereum ./json-rpc)

(define-type NonceOperation
  (Enum Peek Next Reset))

(.def (NonceTracker @ [(Fun (Maybe Quantity) <- NonceOperation) DebugPersistentActivity]
       <-key)
   sexp: 'NonceTracker
   .element?: procedure?
   Key: Address
   key-prefix: (string->bytes "ETNT")
   State: (Maybe Quantity)
   ;; Initial state: unknown, to be resynchronized
   make-default-state: void
   .restore:
   (lambda (address save! nonce _tx)
     (def (reset)
       (set! nonce
         (retry
          retry-window: 0.01
          max-window: 5.0
          max-retries: +inf.0
          (cut eth_getTransactionCount address 'latest))))
     (def (continue result n)
       (with-committed-tx (tx) (set! nonce n) (save! nonce tx))
       result)
     (def (next n)
       (continue n (1+ n)))
     (sequentialize
      ['nonce-tracker address]
      (lambda (op)
        (match op
          ('Reset (continue null null))
          ('Peek (when (eq? nonce null) (reset)) nonce)
          ('Next (when (eq? nonce null) (reset)) (next nonce))))))
   reset: (lambda (x) ((<-key x) 'Reset))
   peek: (lambda (x) ((<-key x) 'Peek))
   next: (lambda (x) ((<-key x) 'Next))
   sync: (lambda (x) ((<-key x) 'Sync)))


(defstruct (NonceTooLow exception) ())

;; TODO: Send Notification to end-user via UI!
;; : Bottom <- Address
(def (nonce-too-low address)
  (.call NonceTracker reset address)
  (raise (NonceTooLow)))

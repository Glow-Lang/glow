(export #t)

(import
  :gerbil/gambit/threads
  :std/error :std/misc/completion :std/text/hex
  :clan/utils/base :clan/utils/concurrency :clan/utils/failure :clan/utils/option :clan/utils/maybe
  :clan/net/json-rpc
  :clan/poo/poo :clan/poo/brace :clan/poo/io
  :clan/runtime/db :clan/runtime/persist
  ./types ./ethereum ./json-rpc ./transaction)

(define-type NonceOperation
  (Enum Peek Next Reset))

(.def (NonceTracker @ PersistentActivity get)
   repr: 'NonceTracker
   .element?: false
   Context: Unit
   Key: Address
   key-prefix: (string->bytes "ETNT")
   State: (Maybe Quantity)
   ;; zero is often wrong, but just let it fail and resynchronize
   make-default-state: void
   ;;T: (Fun Quantity <- NonceOperation)
   make-activity:
   (lambda (address save! nonce)
     (def (reset)
       (set! nonce
         (retry
          retry-window: 0.01
          max-window: 5.0
          max-retries: +inf.0
          (cut eth_getTransactionCount address 'latest))))
     (def (continue result n tx)
       (set! nonce n)
       (save! nonce tx)
       result)
     (def (next n tx)
       (continue n (1+ n) tx))
     (sequentialize
      ['nonce-tracker address]
      (lambda (op tx)
        (match op
          ('Reset (continue null null tx))
          ('Peek (when (eq? nonce null) (reset)) nonce)
          ('Next (when (eq? nonce null) (reset)) (next nonce tx))
          ('Sync (sync-transaction tx))))))
   reset: (lambda (x tx) ((get x) 'Reset tx))
   peek: (lambda (x) ((get x) 'Peek #f))
   next: (lambda (x tx) ((get x) 'Next tx))
   sync: (lambda (x tx) ((get x) 'Sync tx)))


(defstruct (NonceTooLow exception) ())

;; TODO: Send Notification to end-user via UI!
;; : Bottom <- Address
(def (nonce-too-low address)
  (.call NonceTracker reset address)
  (raise (NonceTooLow)))

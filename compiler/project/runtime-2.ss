(export #t
        (import:
          :std/sugar
          :std/misc/channel
          :gerbil/gambit/threads
          :clan/base
          :clan/poo/object
          :clan/poo/type
          :clan/concurrency
          :clan/crypto/random
          :clan/crypto/secp256k1
          :mukn/ethereum/types
          :mukn/ethereum/ethereum
          :mukn/ethereum/known-addresses))

(import :std/sugar
        :std/format
        :std/iter
        :std/text/json
        :std/misc/list
        :std/misc/number
        :std/misc/channel
        :gerbil/gambit/threads
        (only-in :gerbil/gambit/ports output-port-readtable output-port-readtable-set! read-u8)
        (only-in :gerbil/gambit/readtables readtable-sharing-allowed?-set)
        :clan/base
        :clan/pure/dict/assq
        :clan/concurrency
        :clan/crypto/random
        :clan/crypto/secp256k1
        :clan/poo/object
        :clan/poo/io
        :clan/persist/content-addressing
        (only-in :clan/poo/type Sum define-sum-constructors)
        :clan/pure/dict/dicteq
        :mukn/glow/compiler/syntax-context
        :mukn/ethereum/types
        :mukn/ethereum/ethereum
        :mukn/ethereum/known-addresses)

(def (set-caddr!  p v) (set-car! (cddr  p) v))
(def (set-cadddr! p v) (set-car! (cdddr p) v))

;; A Message is a:
;;   (list 'message Address Bytes [Assqof (U #f Address) Int])
(def (message sender published asset-transfers)
  ['message sender published asset-transfers])
(def message-sender cadr)
(def message-published caddr)
(def message-asset-transfers cadddr)
(def message-asset-transfers-set! set-cadddr!)

;; A MessageOut is a:
;;   (list 'message Address OutputPort [Assqof (U #f Address) Int])
;; A MessageIn is a:
;;   (list 'message Address InputPort [Assqof (U #f Address) Int])

;; open-message-out : Address -> MessageOut
(def (open-message-out sender)
  (message sender (open-output-u8vector) []))

;; get-output-message : MessageOut -> Message
(def (get-output-message m)
  (def published-out (message-published m))
  (def published-bytes (get-output-u8vector published-out))
  (close-output-port published-out)
  (message (message-sender m) published-bytes (message-asset-transfers m)))

;; open-message-in : Message -> MessageIn
(def (open-message-in m)
  (message (message-sender m)
           (open-input-u8vector (message-published m))
           (message-asset-transfers m)))

;; close-message-in : MessageIn -> Void
(def (close-message-in m)
  (unless (eof-object? (read-u8 (message-published m)))
    (error 'close-message-in "published not eof:" (message-published m)))
  (close-input-port (message-published m))
  (unless (andmap zero? (assq-values (message-asset-transfers m)))
    (error 'close-message-in "asset-transfers:" (message-asset-transfers m))))

;; current-balances : [Parameterof [Dicteqof Address Int]]
(def current-balances (make-thread-parameter #f))

;; current-receiving-message : [Parameterof MessageIn]
;; current-received-message : [Parameterof Message]
(def current-receiving-message (make-thread-parameter #f))
(def current-received-message (make-thread-parameter #f))

;; current-in-progress-message : [Parameterof MessageOut]
(def current-in-progress-message (make-thread-parameter #f))

;; --------------------------------------------------------

;; A Channel/s is one of:
;;  - Channel
;;  - [Listof Channel]

;; current-address : [Parameterof Address]
(def current-address (make-thread-parameter #f))

;; current-input-channel : [Parameterof Channel]
;; current-output-channel : [Parameterof Channel/s]
(def current-input-channel (make-thread-parameter #f))
(def current-output-channel (make-thread-parameter #f))

;; participant:set-participant : Address -> Void
(def (participant:set-participant p)
  (def me (current-address))
  (def participant->consensus (current-output-channel))
  (def consensus->participant (current-input-channel))
  (def rcv (current-receiving-message))
  (def prg (current-in-progress-message))
  (cond
    ((equal? p me) ; I'm next up
     (cond
       (prg  (void)) ; already me, carry on
       (rcv          ; your turn's over, my turn now
        (participant-done-processing-message)
        (participant-new-in-progress-message))
       (else (participant-new-in-progress-message)))) ; I'll go first
    (prg  ; my turn's over, you're up
     (participant-send-in-progress-message participant->consensus consensus->participant)
     (participant-expect-message consensus->participant)
     (assert! (equal? p (message-sender (current-receiving-message)))))
    (rcv  ; same or third?
     (cond
       ((equal? p (message-sender rcv)) (void)) ; same, carry on
       (else ; third, other over to other other
        (participant-done-processing-message)
        (participant-expect-message consensus->participant)
        (assert! (equal? p (message-sender (current-receiving-message)))))))
    (else ; no, you go first
     (participant-expect-message consensus->participant)
     (assert! (equal? p (message-sender (current-receiving-message)))))))

;; consensus:set-participant : Address -> Void
(def (consensus:set-participant p)
  (def participant->consensus (current-input-channel))
  (def consensus->participants (current-output-channel))
  (def rcv (current-receiving-message))
  (cond
    (rcv  ; same or other?
     (cond
       ((equal? p (message-sender rcv)) (void)) ; same, carry on
       (else ; your turn's over, other's turn now
        (consensus-done-processing-message consensus->participants)
        (consensus-receive-message [p] participant->consensus))))
    (else ; you go first
     (consensus-receive-message [p] participant->consensus))))

;; participant:end-interaction : -> Void
(def (participant:end-interaction)
  (def participant->consensus (current-output-channel))
  (def consensus->participant (current-input-channel))
  (def rcv (current-receiving-message))
  (def prg (current-in-progress-message))
  (cond
    (prg  ; my turn's over, I'm last
     (participant-send-in-progress-message participant->consensus consensus->participant))
    (rcv  ; your turn's over, you're last
     (participant-done-processing-message))
    (else ; nothing was never anywhere
     (void))))

;; consensus:end-interaction : -> Void
(def (consensus:end-interaction)
  (def participant->consensus (current-input-channel))
  (def consensus->participants (current-output-channel))
  (def rcv (current-receiving-message))
  (cond
    (rcv  ; your turn's over, you're last
     (consensus-done-processing-message consensus->participants))
    (else ; nothing was never anywhere
     (void))))

;; participant:withdraw : Address Nat -> Void
(def (participant:withdraw p n)
  (def prg (current-in-progress-message))
  (cond
    (prg (add-to-withdraw p n))
    (else (expect-withdrawn p n))))

;; consensus:withdraw : Address Nat -> Void
(def (consensus:withdraw p n)
  (expect-withdrawn p n))

;; --------------------------------------------------------

;; participant-new-in-progress-message : -> Void
;; Used by the participant who will send the next message
(def (participant-new-in-progress-message)
  (awhen (msg (current-in-progress-message))
    (error 'participant-new-in-progress-message "expected current-in-progress-message #f, given" msg))
  (current-in-progress-message (open-message-out (current-address))))

;; participant-send-in-progress-message : Channel Channel -> Void
;; Used by the participant sending a message to the consesus
(def (participant-send-in-progress-message participant->consensus consensus->participant)
  (def msg (get-output-message (current-in-progress-message)))
  ;; calculate updated balances, but don't set current-balances yet
  (update-balances (current-balances) (message-asset-transfers msg))
  (channel-put participant->consensus msg)
  (current-in-progress-message #f)
  ;; this will set current-balances to the updated balances
  (participant-expect-message consensus->participant)
  (assert! (equal? msg (current-received-message)))
  (current-receiving-message #f))

;; consensus-receive-message : [Listof MPart] Channel -> Void
;; Used by the consensus receiving from a participant
(def (consensus-receive-message nexts participant->consensus)
  (assert! (not (current-receiving-message)))
  (def msg (channel-get participant->consensus))
  (def sender (message-sender msg))
  (unless (or (member sender nexts) (member #f nexts))
    (error "sender " sender " does not match nexts " nexts))
  (current-received-message msg)
  (current-receiving-message (open-message-in msg)))

;; consensus-done-processing-message : [Listof Channel] -> Void
;; Used by the consensus after the transition to the next checkpoint
(def (consensus-done-processing-message consensus->participants)
  ;; TODO: Save the frame, using the set of live variables
  (awhen (msg (current-receiving-message))
    (close-message-in msg)
    ; update balances and async send to each participant
    (let ((orig-msg (current-received-message)))
      (update-current-balances (message-asset-transfers orig-msg))
      (define p (current-output-port))
      (output-port-readtable-set!
        p
        (readtable-sharing-allowed?-set (output-port-readtable p) #f))
      (printf "consensus confirmed message:\n~y" orig-msg)
      (for-each (cut channel-put <> orig-msg) consensus->participants))
    (current-receiving-message #f)
    (current-received-message #f)))

;; participant-expect-message : Channel -> Void
;; Used by a participant expecting to be notified of another's message
(def (participant-expect-message consensus->participant)
  (awhen (msg (current-receiving-message))
    (error 'participant-expect-message "should be no previous receiving-message" msg))
  (def msg (channel-get consensus->participant))
  (update-current-balances (message-asset-transfers msg))
  (current-received-message msg)
  (current-receiving-message (open-message-in msg)))

;; participant-done-processing-message : -> Void
(def (participant-done-processing-message)
  (awhen (msg (current-receiving-message))
    (close-message-in msg)
    (current-receiving-message #f)))

;; --------------------------------------------------------

;; add-to-publish : Sym Any TypeMethods -> Void
(def (add-to-publish x v t)
  ;; ignore x, by order not by name
  (marshal t v (message-published (current-in-progress-message))))

;; expect-published : Sym TypeMethods -> Any
(def (expect-published x t)
  ;; ignore x, by order not by name
  (unmarshal t (message-published (current-receiving-message))))

;; add-to-deposit : Nat -> Void
(def (add-to-deposit n)
  (def msg (current-in-progress-message))
  (def p (message-sender msg))
  (def mat (message-asset-transfers msg))
  (def mat2 (assq-update mat p  (cut - <> n) 0))
  (def mat3 (assq-update mat2 #f (cut + <> n) 0))
  (set! (message-asset-transfers msg) mat3))

;; expect-deposited : Nat -> Void
(def (expect-deposited n)
  (def msg (current-receiving-message))
  (def p (message-sender msg))
  (def mat (message-asset-transfers msg))
  (def mat2 (assq-update mat p  (cut + <> n) 0))
  (def mat3 (assq-update mat2 #f (cut - <> n) 0))
  (set! (message-asset-transfers msg) mat3))

;; add-to-withdraw : Address Nat -> Void
(def (add-to-withdraw p n)
  (define msg (current-in-progress-message))
  (def mat (message-asset-transfers msg))
  (def mat2 (assq-update mat p  (cut + <> n) 0))
  (def mat3 (assq-update mat2 #f (cut - <> n) 0))
  (set! (message-asset-transfers msg) mat3))

;; expect-withdrawn : Address Nat -> Void
(def (expect-withdrawn p n)
  (define msg (current-receiving-message))
  (def mat (message-asset-transfers msg))
  (def mat2 (assq-update mat p  (cut - <> n) 0))
  (def mat3 (assq-update mat2 #f (cut + <> n) 0))
  (set! (message-asset-transfers msg) mat3))

;; --------------------------------------------------------

;; get-balance : Address -> Nat
(def (get-balance p)
  ; TODO: (eth_getBalance p 'pending) from :mukn/ethereum/json-rpc
  1)

;; get-balances : [Listof Address] -> [Dicteqof Address Nat]
(def (get-balances ps)
  (list->dicteq
   (for/collect ((p ps))
     (cons p (get-balance p)))))

;; update-balances : [Assqof Address Int] -> [Dicteqof Address Nat]
(def (update-balances before transfers)
  (for/fold (bal before) ((p transfers))
    (with (([k . v0] p))
      (def v1 (+ (dicteq-ref bal k (lambda () 0)) v0))
      (unless (<= 0 v1)
        (error 'update-balances "balance cannot go below 0" k v1))
      (dicteq-put bal k v1))))

;; update-current-balances : [Hashof Address Int] -> Void
(def (update-current-balances transfers)
  (current-balances (update-balances (current-balances) transfers)))

;; --------------------------------------------------------

(def == equal?)

(def mod modulo)

(def (digest alst)
  (def out (open-output-u8vector))
  (for ((p alst))
    (with (([t . v] p)) (marshal t v out)))
  (digest<-bytes (get-output-u8vector out)))

(def (input t s)
  (printf "input ~s: ~a\n" (.@ t sexp) (if (u8vector? s) (bytes->string s) s))
  (<-json t (read-json (current-input-port))))

;; isValidSignature : Address Digest Signature -> Bool
(def (isValidSignature address digest signature)
  (message-signature-valid? address signature digest))

;; sign : Digest -> Signature
(def (sign digest)
  (make-message-signature (secret-key<-address (current-address)) digest))

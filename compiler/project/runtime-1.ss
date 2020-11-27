(export #t
        (import:
          :std/sugar
          :std/misc/channel
          :gerbil/gambit/threads
          :clan/base
          :clan/poo/poo
          :clan/poo/type
          :clan/concurrency
          :clan/pure/dict/dicteq
          :mukn/ethereum/types
          :mukn/ethereum/known-addresses))

(import :std/sugar
        :std/format
        :std/iter
        :std/text/json
        :std/misc/list
        :std/misc/number
        :std/misc/channel
        :gerbil/gambit/threads
        ;; TODO: use more cryptographically-secure randomness
        ;; from Gerbil :std/crypto or fare/gerbil-crypto
        (only-in :gerbil/gambit/random random-integer)
        :clan/base
        :clan/concurrency
        :clan/poo/poo
        (only-in :clan/poo/type Sum define-sum-constructors)
        :clan/poo/io
        :clan/persist/content-addressing
        :clan/pure/dict/dicteq
        :mukn/glow/compiler/syntax-context
        :mukn/ethereum/types
        :mukn/ethereum/known-addresses
        :mukn/ethereum/signing)

;; A Message is a:
;;   (message Address [Listof (cons Sym Any)] [Hashof Address Int])
(defstruct message (sender published asset-transfers) transparent: #t)
;; An InProgressMessage has the `published` field reversed
;; message-copy : Message -> Message
(def (message-copy m)
  (message (message-sender m)
           (message-published m)
           (hash-copy (message-asset-transfers m))))
;; finalize-message : InProgressMessage -> Message
(def (finalize-message m)
  (message (message-sender m)
           (reverse (message-published m))
           (message-asset-transfers m)))

;; current-balances : [Parameterof [Dicteqof Address Int]]
(def current-balances (make-thread-parameter #f))

;; current-receiving-message : [Parameterof Message]
(def current-receiving-message (make-thread-parameter #f))
(def current-received-message (make-thread-parameter #f))

;; current-in-progress-message : [Parameterof InProgressMessage]
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
  (current-in-progress-message (message (current-address) [] (make-hash-table-eq))))

;; participant-send-in-progress-message : Channel Channel -> Void
;; Used by the participant sending a message to the consesus
(def (participant-send-in-progress-message participant->consensus consensus->participant)
  (def msg (finalize-message (current-in-progress-message)))
  ;; calculate updated balances, but don't set current-balances yet
  (update-balances (current-balances) (message-asset-transfers msg))
  (channel-put participant->consensus msg)
  (current-in-progress-message #f)
  ;; this will set current-balances to the updated balances
  (participant-expect-message consensus->participant)
  (assert! (equal? msg (current-receiving-message)))
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
  (current-receiving-message (message-copy msg)))

;; consensus-done-processing-message : [Listof Channel] -> Void
;; Used by the consensus after the transition to the next checkpoint
(def (consensus-done-processing-message consensus->participants)
  ;; TODO: Save the frame, using the set of live variables
  (awhen (msg (current-receiving-message))
    (unless (null? (message-published msg))
      (error 'consensus-done-processing-message "published:" (message-published msg)))
    (unless (andmap zero? (hash-values (message-asset-transfers msg)))
      (error 'consensus-done-processing-message "asset-transfers:" (message-asset-transfers msg)))
    ; update balances and async send to each participant
    (let ((orig-msg (current-received-message)))
      (update-current-balances (message-asset-transfers orig-msg))
      (printf "consensus confirmed message:\n~r\n" orig-msg)
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
  (current-receiving-message msg))

;; participant-done-processing-message : -> Void
(def (participant-done-processing-message)
  (awhen (msg (current-receiving-message))
    (unless (null? (message-published msg))
      (error 'participant-done-processing-message "published:" (message-published msg)))
    (unless (andmap zero? (hash-values (message-asset-transfers msg)))
      (error 'participant-done-processing-message "asset-transfers:" (message-asset-transfers msg)))
    (current-receiving-message #f)))

;; --------------------------------------------------------

;; add-to-publish : Sym Any -> Void
(def (add-to-publish x v)
  (def msg (current-in-progress-message))
  (push! (cons x v) (message-published msg)))

;; expect-published : Sym -> Any
(def (expect-published x)
  (def msg (current-receiving-message))
  (def p (pop! (message-published msg)))
  (unless (and (pair? p) (eq? (car p) x))
    (error 'expect-published x p msg))
  (cdr p))

;; add-to-deposit : Nat -> Void
(def (add-to-deposit n)
  (def msg (current-in-progress-message))
  (def p (message-sender msg))
  (def mat (message-asset-transfers msg))
  (hash-update! mat p  (cut - <> n) 0)
  (hash-update! mat #f (cut + <> n) 0))

;; expect-deposited : Nat -> Void
(def (expect-deposited n)
  (def msg (current-receiving-message))
  (def p (message-sender msg))
  (def mat (message-asset-transfers msg))
  (hash-update! mat p  (cut + <> n) 0)
  (hash-update! mat #f (cut - <> n) 0))

;; add-to-withdraw : Address Nat -> Void
(def (add-to-withdraw p n)
  (def mat (message-asset-transfers (current-in-progress-message)))
  (hash-update! mat p  (cut + <> n) 0)
  (hash-update! mat #f (cut - <> n) 0))

;; expect-withdrawn : Address Nat -> Void
(def (expect-withdrawn p n)
  (def mat (message-asset-transfers (current-receiving-message)))
  (hash-update! mat p  (cut - <> n) 0)
  (hash-update! mat #f (cut + <> n) 0))

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

;; update-balances : [Hashof Address Int] -> [Dicteqof Address Nat]
(def (update-balances before transfers)
  (for/fold (bal before) ((p (hash->list transfers)))
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

(def (randomUInt256)
  (random-integer (expt 2 256)))

(def (digest alst)
  (def out (open-output-u8vector))
  (for ((p alst))
    (with (([t . v] p)) (marshal t v out)))
  (digest<-bytes (get-output-u8vector out)))

(def (input t s)
  (printf "input ~s: ~a\n" (.@ t sexp) s)
  (<-json t (read-json (current-input-port))))

;; isValidSignature : Address Digest Signature -> Bool
(def (isValidSignature address digest signature)
  (message-signature-valid? address signature digest))

;; sign : Digest -> Signature
(def (sign digest)
  (make-message-signature (secret-key<-address (current-address)) digest))

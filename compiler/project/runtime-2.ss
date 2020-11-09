(export #t
        (import:
          :std/sugar
          :std/misc/channel
          :gerbil/gambit/threads
          :clan/concurrency
          :mukn/ethereum/known-addresses
          :mukn/ethereum/signing))

(import :std/sugar
        :std/format
        :std/misc/list
        :std/misc/number
        :std/misc/channel
        :gerbil/gambit/threads
        (only-in :gerbil/gambit/ports output-port-readtable output-port-readtable-set!)
        (only-in :gerbil/gambit/readtables readtable-sharing-allowed?-set)
        :clan/pure/dict/assq
        :clan/concurrency
        :clan/poo/io
        :mukn/glow/compiler/syntax-context
        :mukn/ethereum/known-addresses
        :mukn/ethereum/signing)

(def (set-caddr!  p v) (set-car! (cddr  p) v))
(def (set-cadddr! p v) (set-car! (cdddr p) v))
(def (assq-values a) (map cdr a))

;; A Message is a:
;;   (list 'message Address [Listof (cons Sym Bytes)] [Assqof (U #f Address) Int])
(def (message sender published asset-transfers)
  ['message sender published asset-transfers])
(def message-sender cadr)
(def message-published caddr)
(def message-published-set! set-caddr!)
(def message-asset-transfers cadddr)
(def message-asset-transfers-set! set-cadddr!)
;; An InProgressMessage has the `published` field reversed
;; message-copy : Message -> Message
(def (message-copy m)
  (message (message-sender m)
           (message-published m)
           (message-asset-transfers m)))
;; finalize-message : InProgressMessage -> Message
(def (finalize-message m)
  (message (message-sender m)
           (reverse (message-published m))
           (message-asset-transfers m)))

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

;; --------------------------------------------------------

;; participant-new-in-progress-message : -> Void
;; Used by the participant who will send the next message
(def (participant-new-in-progress-message)
  (awhen (msg (current-in-progress-message))
    (error 'participant-new-in-progress-message "expected current-in-progress-message #f, given" msg))
  (current-in-progress-message (message (current-address) [] [])))

;; participant-send-in-progress-message : Channel Channel -> Void
;; Used by the participant sending a message to the consesus
(def (participant-send-in-progress-message participant->consensus consensus->participant)
  (def msg (finalize-message (current-in-progress-message)))
  (channel-put participant->consensus msg)
  (current-in-progress-message #f)
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
    (unless (andmap zero? (assq-values (message-asset-transfers msg)))
      (error 'consensus-done-processing-message "asset-transfers:" (message-asset-transfers msg)))
    ; async send to each participant
    (let ((orig-msg (current-received-message)))
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
  (current-receiving-message (channel-get consensus->participant)))

;; participant-done-processing-message : -> Void
(def (participant-done-processing-message)
  (awhen (msg (current-receiving-message))
    (unless (null? (message-published msg))
      (error 'participant-done-processing-message "published:" (message-published msg)))
    (unless (andmap zero? (assq-values (message-asset-transfers msg)))
      (error 'participant-done-processing-message "asset-transfers:" (message-asset-transfers msg)))
    (current-receiving-message #f)))

;; --------------------------------------------------------

;; add-to-publish : Sym Any TypeMethods -> Void
(def (add-to-publish x v t)
  (def msg (current-in-progress-message))
  (def ent (cons x (bytes<- t v)))
  (push! ent (message-published msg)))

;; expect-published : Sym TypeMethods -> Any
(def (expect-published x t)
  (def msg (current-receiving-message))
  (def p (pop! (message-published msg)))
  (unless (and (pair? p) (eq? (car p) x))
    (error 'expect-published x p msg))
  (<-bytes t (cdr p)))

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

;; isValidSignature : Address Digest Signature -> Bool
(def (isValidSignature address digest signature)
  (message-signature-valid? address signature digest))

;; sign : Digest -> Signature
(def (sign digest)
  (make-message-signature (secret-key<-address (current-address)) digest))

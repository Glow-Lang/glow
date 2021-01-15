(export #t)

(import
  :std/misc/list :clan/base :clan/poo/io :clan/pure/dict/assq)

;; MESSAGE ;; TODO: more like CommunicationState
;; TODO: Split this into ActiveBlockCtx and PassiveBlockCtx
(defclass Message
  (inbox ;; : BytesInputPort
   outbox ;; : ListOf DependentPair ;; TODO: just have a BytesOutputPort ?
   asset-transfers) ;; : (Alist Z <- Address)
  constructor: :init!
  transparent: #t)

(defmethod {:init! Message}
  (λ (self (i #f) (o []) (at []))
    (set! (@ self inbox) i)
    (set! (@ self outbox) o)
    (set! (@ self asset-transfers) at)))

;; <- Message
(defmethod {reset Message}
  (λ (self)
    (set! (@ self inbox) #f)
    (set! (@ self outbox) [])))

;; <- Message Symbol t:Type t
(defmethod {add-to-published Message}
  (λ (self name type value)
    (def dependent-pair [type . value])
    (set! (@ self outbox) (snoc dependent-pair (@ self outbox)))))

;; expect-published : t <- Symbol t:Type
(defmethod {expect-published Message}
  (λ (self name type)
    ;; ignore name, by order not by name
    (unmarshal type (@ self inbox))))

;; add-to-withdraw : <- Address Nat
(defmethod {add-to-withdraw Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut + <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut - <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; add-to-deposit : <- Nat
(defmethod {add-to-deposit Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut - <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut + <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; expect-deposited : <- Nat
(defmethod {expect-deposited Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut + <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut - <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; expect-withdrawn : <- Address Nat
(defmethod {expect-withdrawn Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut - <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut + <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; : Nat <- Message Symbol
(defmethod {compute-participant-dues Message}
  (λ (self participant)
    (def asset-transfers (@ self asset-transfers))
    (def active-balance
      (if (assq-has-key? asset-transfers participant)
        (assq-ref asset-transfers participant)
        0))
    (if (negative? active-balance)
      (abs active-balance)
      0)))

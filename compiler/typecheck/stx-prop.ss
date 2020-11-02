
(export set-is-type
        get-is-type
        track-is-type
        set-has-typing-scheme
        get-has-typing-scheme
        track-has-typing-scheme
        make-has-type-table
        current-has-type-table
        set-has-type!
        get-has-type)

(import <expander-runtime>)

(def is-type-table (make-hash-table-eq weak-keys: #t))
(def has-typing-scheme-table (make-hash-table-eq weak-keys: #t))

;; set-is-type : Stx Type -> Stx
(def (set-is-type stx t)
  (hash-put! is-type-table stx t)
  stx)

;; get-is-type : Stx -> MaybeType
(def (get-is-type stx) (hash-get is-type-table stx))

;; track-is-type : Stx Stx -> Stx
;; Atteches the is-type of `old` onto `new`, returning `new`.
(def (track-is-type old new)
  (def old-is-type (get-is-type old))
  (if old-is-type (set-is-type new old-is-type) new))

;; --------------------------------------------------------

;; set-has-typing-scheme : Stx TypingScheme -> Stx
(def (set-has-typing-scheme stx ts)
  (hash-put! has-typing-scheme-table stx ts)
  stx)

;; get-has-typing-scheme : Stx -> MaybeTypingScheme
(def (get-has-typing-scheme stx) (hash-get has-typing-scheme-table stx))

;; track-has-typing-scheme : Stx Stx -> Stx
;; Atteches the has-type of `old` onto `new`, returning `new`.
(def (track-has-typing-scheme old new)
  (def old-has-typing-scheme (get-has-typing-scheme old))
  (if old-has-typing-scheme (set-has-typing-scheme new old-has-typing-scheme) new))

;; --------------------------------------------------------

(def (make-has-type-table) (make-hash-table))
(def current-has-type-table (make-parameter (make-has-type-table)))
(def (set-has-type! e t)
  (hash-put! (current-has-type-table) (syntax->datum e) t))
(def (get-has-type e)
  (hash-get (current-has-type-table) (syntax->datum e)))



(export set-is-type
        get-is-type
        track-is-type)

(def is-type-table (make-hash-table-eq weak-keys: #t))

;; set-is-type : Stx Type -> Stx
(def (set-is-type stx t)
  (hash-put! is-type-table stx t)
  stx)

;; get-is-type : Stx -> MaybeType
(def (get-is-type stx) (hash-get is-type-table stx))

;; track-is-type : Stx Stx -> Stx
(def (track-is-type old new)
  (def old-is-type (get-is-type old))
  (if old-is-type (set-is-type new old-is-type) new))

;; --------------------------------------------------------
;; Not implemented, or not needed yet

;; set-has-type : Stx Type -> Stx
;; get-has-type : Stx -> MaybeType
;; track-has-type : Stx Stx -> Stx

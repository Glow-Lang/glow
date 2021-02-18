(export (struct-out entry entry-val entry-ctor entry-type)
        env-put/env
        bound-as-ctor?
        not-bound-as-ctor?
        bound-as-type?
        symbol-refer
        current-debug-label-table
        make-debug-label-table
        add-debug-label!)

(import
  <expander-runtime>
  :clan/pure/dict/symdict)

;; An Env is a [Symdictof Entry]
;; An Entry is an (entry Symbol), one of:
;;  - (entry-val Symbol)
;;  - (entry-ctor Symbol)
;;  - (entry-type Symbol)
(defstruct entry (sym) transparent: #t)
(defstruct (entry-val entry) () transparent: #t)
(defstruct (entry-ctor entry) () transparent: #t)
(defstruct (entry-type entry) () transparent: #t)

;; env-put/env : Env Env -> Env
;; entries in the 2nd env override ones in the 1st
(def (env-put/env e1 e2) (symdict-put/list e1 (symdict->list e2)))

;; bound-as-ctor? : Env Symbol -> Bool
(def (bound-as-ctor? env s)
  (and (symdict-has-key? env s)
       (entry-ctor? (symdict-ref env s))))

;; not-bound-as-ctor? : Env Symbol -> Bool
(def (not-bound-as-ctor? env s)
  (or (not (symdict-has-key? env s))
      (not (entry-ctor? (symdict-ref env s)))))

;; bound-as-type? : Env Symbol -> Bool
(def (bound-as-type? env s)
  (and (symdict-has-key? env s)
       (entry-type? (symdict-ref env s))))

;; symbol-refer : Env Symbol -> Symbol
;; looks up the symbol in the env
(def (symbol-refer env s)
  (unless (symdict-has-key? env s)
    (error 'alpha-convert "unbound identifier" s))
  (entry-sym (symdict-ref env s)))

;; A DebugLabelTable is a [Hashof Symbol Env]
(def current-debug-label-table (make-parameter #f))
(def (make-debug-label-table) (make-hash-table-eq))

;; add-debug-label! : Identifier Env -> Void
(def (add-debug-label! dlb env)
  (hash-put! (current-debug-label-table) (stx-e dlb) env))


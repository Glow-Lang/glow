(export #t)

(import
  :clan/pure/dict/symdict)

;; An Env is a [Symdictof Entry]
;; An Entry is an (entry Symbol Bool)
(defstruct entry (sym ctor?) transparent: #t)

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

;; symbol-refer : Env Symbol -> Symbol
;; looks up the symbol in the env
(def (symbol-refer env s)
  (unless (symdict-has-key? env s)
    (error 'alpha-convert "unbound identifier" s))
  (entry-sym (symdict-ref env s)))

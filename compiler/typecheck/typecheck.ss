(export typecheck
        (struct-out entry:unknown entry:known entry:ctor entry:type)
        read-type-env-file
        write-type-env
        type-env=?
        (import:
         :mukn/glow/compiler/common
         :mukn/glow/compiler/typecheck/variance
         :mukn/glow/compiler/typecheck/type))

(import
  :gerbil/gambit
  :gerbil/expander
  :std/format
  :std/iter
  (only-in :std/srfi/1 map-in-order)
  :std/misc/list
  :std/misc/repr
  :clan/pure/dict/assq
  :clan/pure/dict/symdict
  :clan/debug ;; TODO: remove after debugging
  :mukn/glow/compiler/syntax-context
  (for-template :mukn/glow/compiler/syntax-context)
  :mukn/glow/compiler/common
  :mukn/glow/compiler/alpha-convert/env
  :mukn/glow/compiler/alpha-convert/fresh
  :mukn/glow/compiler/typecheck/variance
  :mukn/glow/compiler/typecheck/type
  :mukn/glow/compiler/typecheck/stx-prop)

(def map map-in-order) ;; I suspect side-effects make order important
;; Typechecking glow-sexpr

;; Type system and scoping rules are closest
;; to ReScript, but Record types are structural.

;; A SymbolNTypeTable is a [Hashof Symbol NType]
;; Keys are symbols from unannotated lambda-bound identifiers in the program.
;; These have been alpha-renamed to be unique beforehand.
;; Values are the types associated with those identifiers after type inference.
;; They will be negative types because unannotated lambda-bound variables.
;; make-symbol-ntype-table : -> SymbolTypeTable
(def (make-symbol-ntype-table) (make-hash-table-eq))

;; current-symbol-ntype-table : [Parameterof SymbolNTypeTable]
(def current-symbol-ntype-table (make-parameter (make-symbol-ntype-table)))

;; put-ntype! : Symbol NType -> Void
;; Adds the symbol and type to the current-symbol-ntype-table
(define (put-ntype! s t)
  (hash-put! (current-symbol-ntype-table) s t))

;; type=? : Type Type -> Bool
(def (type=? a b)
  (and (subtype? a b) (subtype? b a)))

;; variance-type~? : Variance Type Type -> Bool
;; (~? irrelevant a b) = #t
;; (~? covariant a b) = (subtype? a b)
;; (~? contravariant a b) = (subtype? b a)
;; (~? invariant a b) = (type=? a b)
(def (variance-type~? v a b)
  (and (or (variance-covariant? v) (subtype? b a))
       (or (variance-contravariant? v) (subtype? a b))))

;; variance-type-pair~? : Variance TypeVariancePair TypeVariancePair -> Bool
(def (variance-type-pair~? v avp bvp)
  (def (?type~? a b)
    (cond ((and a b) (variance-type~? v a b))
          (else (and (not a) (not b)))))
  (with (([an ap] avp) ([bn bp] bvp))
    (and (?type~? bn an) (?type~? ap bp))))

;; subtype? : PType NType -> Bool
(def (subtype? a b)
  (match* (a b)
    (((ptype:union as) b)
     (andmap (lambda (a) (subtype? a b)) as))
    ((a (ntype:intersection bs))
     (andmap (lambda (b) (subtype? a b)) bs))
    (((type:name x) (type:name y))
     (eq? x y))
    ;; cases for name-subtype must be before any possible case for _ <: (union _)
    (((type:name-subtype x a2) (type:name-subtype y b2))
     (cond ((eq? x y) (subtype? a2 b2))
           (else      (subtype? a2 b))))
    (((type:name-subtype _ a2) b)
     (subtype? a2 b))
    (((type:var x) (type:var y)) (eq? x y))
    (((type:tuple as) (type:tuple bs))
     (and (= (length as) (length bs))
          (andmap subtype? as bs)))
    (((type:record as) (type:record bs))
     (symdict=? as bs subtype?))
    (((type:arrow a1s b1) (type:arrow a2s b2))
     (and (equal? (length a1s) (length a2s))
          (andmap subtype? a2s a1s)
          (subtype? b1 b2)))
    (((type:app (type:name f1) a1s) (type:app (type:name f2) a2s))
     (def v1s (type-name-variances f1))
     (and (eq? f1 f2)
          (= (length v1s) (length a1s) (length a2s))
          (andmap variance-type-pair~? v1s a1s a2s)))
    ((_ _) #f)))

;; typing-scheme=? : TypingScheme TypingScheme -> Bool
(def (typing-scheme=? a b)
  (match* (a b)
    (((typing-scheme am at) (typing-scheme bm bt))
     (and (symdict=? am bm type=?) (type=? at bt)))))

;; A Pattys is an [Assqof Symbol Type]
;; for the types of the pattern variables within a pattern
;; TODO: Does order matter? If not, change to a symdict

;; A PatTypingScheme is a (pat-typing-scheme MonoEnv NType Pattys)
(defstruct pat-typing-scheme (menv ntype pattys) transparent: #t)

;; pattys-join : [Listof Pattys] -> Pattys
;; intersection of keys, join of types for same key
;; used for or-patterns
(def (pattys-join ps)
  (cond ((null? ps) [])
        ((null? (cdr ps)) (car ps))
        (else
         (for/collect ((k (assq-keys (car ps)))
                       when
                       (andmap (lambda (o) (assq-has-key? o k)) (cdr ps)))
           (cons k (types-join (map (lambda (p) (assq-ref p k)) ps)))))))

;; pat-typing-schemes-join : [Listof PatTypingScheme] -> PatTypingScheme
;; Meet of menvs, meet of ntypes, join of pattys
;; used for or-patterns
(def (pat-typing-schemes-join pts)
  (def nenv (menvs-meet (map pat-typing-scheme-menv pts)))
  (def ntype (types-meet (map pat-typing-scheme-ntype pts)))
  (def ptype (pattys-join (map pat-typing-scheme-pattys pts)))
  (pat-typing-scheme nenv ntype ptype))

;; pattys-append : [Listof Pattys] -> Pattys
;; append, but check no duplicate keys
;; used for combinations of patterns in the same scope other than "or"
(def (pattys-append ps)
  (def p (apply append ps))
  (check-duplicate-symbols (map car p))
  p)

;; check-duplicate-symbols : [Listof Symbol] -> TrueOrError
;; true if there are no duplicates, error otherwise
(def (check-duplicate-symbols syms)
  (match syms
    ([] #t)
    ([a . bs]
     (if (memq a bs)
         (error a "duplicate identifier")
         (check-duplicate-symbols bs)))))

;; An MPart is one of:
;;  - #f      ; consensus, or no participant
;;  - Symbol  ; within the named participant

;; mpart-can-use? : MPart MPart -> Bool
;; can code with `a` participant priviledges access `b`?
;; any participant can access `#f` consensus identifiers
;; participants can access their own private identifiers
;; the `#f` consensus cannot access participant-specific ids
(def (mpart-can-use? a b) (or (not b) (eq? a b)))

;; mpart->repr-sexpr : MPart -> Sexpr
(def (mpart->repr-sexpr p) (and p `',p))

;; repr-sexpr->mpart : Sexpr -> MPart
(def (repr-sexpr->mpart p) (match p (#f #f) (['quote s] s)))

;; An EnvEntry is one of:
;;  - (entry:unknown MPart)             ; lambda-bound with no type annotation
;;  - (entry:known MPart TypingScheme)  ; let-bound, or lambda-bound with annotation
;;  - (entry:ctor MPart TypingScheme)   ; note ctor does not have N or P, just Type
;;                                      ; and constructor definitions should assign the most permissive "output" type
;;                                      ; because only subtypes of that can be matched against the pattern
;;  - (entry:type MPart [Listof ParamVariancePair] Type)
;; A ParamVariancePair is one of:
;;  - (list #f #f)         ; for irrelevant
;;  - (list #f Symbol)     ; for covariant
;;  - (list Symbol #f)     ; for contravariant
;;  - (list Symbol Symbol) ; for invariant
;; The order follows section 9.1.1 and 9.1.2 of Algebraic Subyping:
;; https://www.cl.cam.ac.uk/~sd601/thesis.pdf#subsection.9.1.1
(defstruct entry:unknown (part) transparent: #t)
(defstruct entry:known (part ts) transparent: #t)
(defstruct entry:ctor (part ts) transparent: #t)
(defstruct entry:type (part params type) transparent: #t)
(def (env-entry? v) (or (entry:unknown? v) (entry:known? v) (entry:ctor? v) (entry:type? v)))
;; xvp+, xvp- : Symbol -> ParamVariancePair
(def (xvp+ x) [#f x]) ; covariant
(def (xvp- x) [x #f]) ; contravariant

;; entry-part : EnvEntry -> MPart
(def (entry-part e)
  (cond ((entry:unknown? e) (entry:unknown-part e))
        ((entry:known? e)   (entry:known-part e))
        ((entry:ctor? e)    (entry:ctor-part e))
        ((entry:type? e)    (entry:type-part e))
        (else               (error 'entry-part "expected an EnvEntry"))))

;; entry-publish : EnvEntry -> EnvEntry
;; Produces a copy of the entry with the MPart to #f, as public
(def (entry-publish e)
  (match e
    ((entry:unknown _)   (entry:unknown #f))
    ((entry:known _ ts)  (entry:known #f ts))
    ((entry:ctor _ ts)   (entry:ctor #f ts))
    ((entry:type _ xvps t) (entry:type #f xvps t))))

;; env-publish : Env Symbol -> Env
;; Produces a new env containing only the published versions of the xs
(def (env-publish env x)
  (unless (symdict-has-key? env x) (error x "unbound identifier"))
  (list->symdict
   [(cons x (entry-publish (symdict-ref env x)))]))

;; An Env is a [Symdictof EnvEntry]
;; Instead of type environments Γ, the reformulated rules use typing
;; environments Π to assign typing schemes (not type schemes) to
;; known-type/let-bound variables.
;; Outside this file, Env should be called TypeEnv.

;; env-put/env : Env Env -> Env
;; entries in the 2nd env override ones in the 1st
(def (env-put/env e1 e2) (symdict-put/list e1 (symdict->list e2)))

;; env-put/envs : Env [Listof Env] -> Env
;; entries later in the list override earlier ones
(def (env-put/envs e1 es2) (for/fold (e e1) (e2 es2) (env-put/env e e2)))

;; param-variance-pair->repr-sexpr : ParamVariancePair -> Sexpr
(def (param-variance-pair->repr-sexpr xvp)
  (def (?->s x) (and x (symbol->repr-sexpr x)))
  (list->repr-sexpr xvp ?->s))

;; repr-sexpr->param-variance-pair : Sexpr -> ParamVariancePair
(def (repr-sexpr->param-variance-pair xvp)
  (def (s->? x) (and x (repr-sexpr->symbol x)))
  (repr-sexpr->list xvp s->?))

;; env-entry->repr-sexpr : EnvEntry -> Sexpr
(def (env-entry->repr-sexpr e)
  (match e
    ((entry:unknown p)   `(entry:unknown ,(mpart->repr-sexpr p)))
    ((entry:known p ts)  `(entry:known ,(mpart->repr-sexpr p) ,(typing-scheme->repr-sexpr ts)))
    ((entry:ctor p ts)   `(entry:ctor ,(mpart->repr-sexpr p) ,(typing-scheme->repr-sexpr ts)))
    ((entry:type p xvps t)
     `(entry:type ,(mpart->repr-sexpr p)
                  ,(list->repr-sexpr xvps param-variance-pair->repr-sexpr)
                  ,(type->repr-sexpr t)))))

;; repr-sexpr->env-entry : Sexpr -> EnvEntry
(def (repr-sexpr->env-entry s)
  (match s
    (['entry:unknown p]  (entry:unknown (repr-sexpr->mpart p)))
    (['entry:known p ts] (entry:known (repr-sexpr->mpart p) (repr-sexpr->typing-scheme ts)))
    (['entry:ctor p ts]  (entry:ctor (repr-sexpr->mpart p) (repr-sexpr->typing-scheme ts)))
    (['entry:type p xvps t]
     (entry:type (repr-sexpr->mpart p)
                 (repr-sexpr->list xvps repr-sexpr->param-variance-pair)
                 (repr-sexpr->type t)))))

;; type-env->repr-sexpr : Env -> Sexpr
(def (type-env->repr-sexpr env) ((symdict->repr-sexpr env-entry->repr-sexpr) env))

;; repr-sexpr->type-env : Sexpr -> Env
(def (repr-sexpr->type-env s) ((repr-sexpr->symdict repr-sexpr->env-entry) s))

(def (read-type-env-file file)
  ;; TODO: move error handling to `run-pass` or `run-passes` in multipass.ss
  (with-catch
   (lambda (e) (display-exception e) #f)
   (lambda ()
     (match (read-syntax-from-file file)
       ([s] (repr-sexpr->type-env (syntax->datum s)))
       (_ (printf "read-type-env-file: expected a single symdict sexpr\n") #f)))))

(def (write-type-env env (port (current-output-port)))
  (when env
    (fprintf port "~y" (type-env->repr-sexpr env))))

;; env-entry=? : EnvEntry EnvEntry -> Bool
(def (env-entry=? a b)
  (match* (a b)
    (((entry:unknown ap) (entry:unknown bp))
     (equal? ap bp))
    (((entry:known ap ats) (entry:known bp bts))
     (and (equal? ap bp) (typing-scheme=? ats bts)))
    (((entry:ctor ap ats) (entry:ctor bp bts))
     (and (equal? ap bp) (typing-scheme=? ats bts)))
    (((entry:type ap axs at) (entry:type bp bxs bt))
     (and (equal? ap bp) (equal? axs bxs) (type=? at bt)))
    ((_ _)
     #f)))

;; type-env=? : Env Env -> Bool
(def (type-env=? a b)
  ;(def ans1 (and a b (symdict=? a b env-entry=?)))
  (def ans2 (and a b (equal? (type-env->repr-sexpr a) (type-env->repr-sexpr b))))
  ;(unless (equal? ans1 ans2)
  ;  (printf "type-env=?: ans1 = ~r, ans2 = ~r\n" ans1 ans2))
  ans2)

;; --------------------------------------------------------

;; A TyvarBisubst is a [Symdictof TypeInterval]
;; A TypeInterval is a (type-interval NType PType)
;; maps negative occurrences of type variables to the NType,
;; and positive occurrences of type variables to the PType
;; should be idempotent: applying a bisubst a 2nd time shouldn't matter
;; should be stable: the NType should be a subtype of the PType
(defstruct type-interval (ntype ptype) transparent: #t)
;; for example applying the bisubst
;; (symdict ('a (type-interval B C)))
;; to the identity function with type ('a -> 'a)
;; results in (B -> C), where a value of type B flows into C

;; not-bound-as-ctor? : Env Symbol -> Bool
(def (not-bound-as-ctor? env s)
  (or (not (symdict-has-key? env s))
      (not (entry:ctor? (symdict-ref env s)))))

;; bound-as-ctor? : Env Symbol -> Bool
(def (bound-as-ctor? env s)
  (and (symdict-has-key? env s)
       (entry:ctor? (symdict-ref env s))))

;; bound-as-type? : Env Symbol -> Bool
(def (bound-as-type? env s)
  (and (symdict-has-key? env s)
       (entry:type? (symdict-ref env s))))

;; tyvar-bisubst : TypeInterval Variance Symbol -> PNType
;; use variance to see which one
(def (tyvar-bisubst tvl v s)
  (cond ((variance-covariant? v) (type-interval-ptype tvl))
        ((variance-contravariant? v) (type-interval-ntype tvl))
        ((equal? (type-interval-ptype tvl) (type-interval-ntype tvl))
         (type-interval-ptype tvl))
        (else (error s "could not infer type"))))

;; type-variance-pair-bisubst : TyvarBisubst Variance TypeVariancePair -> TypeVariancePair
(def (type-variance-pair-bisubst tybi v tvp)
  (with (([tn tp] tvp))
    [(and tn (type-bisubst tybi (variance-flip v) tn))
     (and tp (type-bisubst tybi v tp))]))

;; type-bisubst : TyvarBisubst Variance PNType -> PNType
;; P vs N depends on the variance
(def (type-bisubst tybi v t)
  ;; sub : PNType -> PNType
  (def (sub pt) (type-bisubst tybi v pt))
  ;; vsub : Variance PNType -> PNType
  ;; variance is within v, so must compose with v
  (def (vsub v2 t)
    (type-bisubst tybi (variance-compose v v2) t))
  ;; vsub-pair : Variance TypeVariancePair -> TypeVariancePair
  (def (vsub-pair v2 tvp)
    ;; So v2 should determine what's type/#f in tvp, but not change v
    (type-variance-pair-bisubst tybi v tvp))
  (def (nsub t) (vsub contravariant t))
  (match t
    ((type:name _) t)
    ((type:name-subtype x sup)
     (type:name-subtype x (sub sup)))
    ((type:var s)
     (cond ((symdict-has-key? tybi s)
            (tyvar-bisubst (symdict-ref tybi s) v s))
           (else t)))
    ((type:app (type:name f) as)
     (def vs (type-name-variances f))
     (type:app (type:name f) (map vsub-pair vs as)))
    ((type:tuple as)
     (type:tuple (map sub as)))
    ((type:record fldtys)
     (type:record
      (list->symdict
       (map (lambda (p) (cons (car p) (sub (cdr p))))
            (symdict->list fldtys)))))
    ((type:arrow as b)
     (type:arrow (map nsub as) (sub b)))
    ((ptype:union ts)
     (types-join (map sub ts)))
    ((ntype:intersection ts)
     (types-meet (map sub ts)))))

;; ptype-bisubst : TyvarBisubst PType -> PType
(def (ptype-bisubst tybi ptype) (type-bisubst tybi covariant ptype))

;; ntype-bisubst : TyvarBisubst NType -> NType
(def (ntype-bisubst tybi ntype) (type-bisubst tybi contravariant ntype))

;; menv-bisubst : TyvarBisubst MonoEnv -> MonoEnv
(def (menv-bisubst tybi menv)
  (for/fold (acc empty-symdict) ((k (symdict-keys menv)))
    (symdict-put acc k (ntype-bisubst tybi (symdict-ref menv k)))))

;; typing-scheme-bisubst : TyvarBisubst TypingScheme -> TypingScheme
(def (typing-scheme-bisubst tybi ts)
  (with (((typing-scheme menv ty) ts))
    (typing-scheme (menv-bisubst tybi menv) (ptype-bisubst tybi ty))))

;; pattys-bisubst : TyvarBisubst Pattys -> Pattys
(def (pattys-bisubst tybi patty)
  (for/collect ((e patty))
    (match e
      ((cons k v)
       (cons k (ptype-bisubst tybi v))))))

;; pat-typing-scheme-bisubst : TyvarBisubst PatTypingScheme -> PatTypingScheme
(def (pat-typing-scheme-bisubst tybi pts)
  (with (((pat-typing-scheme menv ntype pattys) pts))
    (pat-typing-scheme (menv-bisubst tybi menv)
                       (ntype-bisubst tybi ntype)
                       (pattys-bisubst tybi pattys))))

;; A Constraints is a [Listof Constraint]
;; A Constraint is one of:
;;  - (constraint:subtype PType NType)
;;  - (constraint:type-equal Type Type)
(defstruct constraint:subtype (a b) transparent: #t)
(defstruct constraint:type-equal (a b) transparent: #t)

;; constraints-from-variance-pair : Variance TypeVariancePair TypeVariancePair -> Constraints
(def (constraints-from-variance-pair v avp bvp)
  (cond ((variance-irrelevant? v) [])
        ((variance-covariant? v)
         (with (([#f ap] avp) ([#f bp] bvp))
           [(constraint:subtype ap bp)]))
        ((variance-contravariant? v)
         (with (([an #f] avp) ([bn #f] bvp))
           [(constraint:subtype bn an)]))
        (else
         (with (([an ap] avp) ([bn bp] bvp))
           [(constraint:subtype ap bp)
            (constraint:subtype bn an)]))))

;; constraints-bisubst : TyvarBisubst Constraints -> Constraints
(def (constraints-bisubst tybi cs)
  (map (lambda (c) (constraint-bisubst tybi c)) cs))

;; constraint-bisubst : TyvarBisubst Constraint -> Constraint
(def (constraint-bisubst tybi c)
  (match c
    ((constraint:subtype a b)
     (constraint:subtype (type-bisubst tybi covariant a)
                         (type-bisubst tybi contravariant b)))
    ((constraint:type-equal a b)
     (constraint:type-equal (type-bisubst tybi invariant a)
                            (type-bisubst tybi invariant b)))))

;; symdict-key-set=? : [Symdict Any] [Symdict Any] -> Bool
(def (symdict-key-set=? a b) (symdict=? a b true))

;; sub-constraints : Constraint -> Constraints
;; correponds to sub_B, non-recursive
;; internal error if the constraint is atomic
;; type error if the constraint is a type mismatch
(def (sub-constraints c)
  (match c
    ((constraint:type-equal a b)
     [(constraint:subtype a b) (constraint:subtype b a)])
    ((constraint:subtype (ptype:union as) b)
     (map (lambda (a) (constraint:subtype a b)) as))
    ((constraint:subtype a (ntype:intersection bs))
     (map (lambda (b) (constraint:subtype a b)) bs))
    ((constraint:subtype (type:name x) (type:name y))
     (unless (eq? x y) (error 'subtype (format "type mismatch, expected ~s, given ~s" y x)))
     [])
    ;; cases for name-subtype must be before any possible case for _ <: (union _)
    ((constraint:subtype (type:name-subtype x a2) (and b (type:name-subtype y b2)))
     (cond ((eq? x y) [(constraint:subtype a2 b2)])
           (else      [(constraint:subtype a2 b)])))
    ((constraint:subtype (type:name-subtype _ a2) b)
     [(constraint:subtype a2 b)])
    ((constraint:subtype (type:name x) (type:name-subtype y _))
     (error 'subtype (format "type mismatch, expected ~s, given ~s" y x)))
    ((constraint:subtype (type:tuple as) (type:tuple bs))
     (unless (= (length as) (length bs))
       (error 'subtype "tuple length mismatch"))
     (map make-constraint:subtype as bs))
    ((constraint:subtype (type:record as) (type:record bs))
     (unless (symdict-key-set=? as bs)
       (error 'subtype (format "record field mismatch, expected keys ~a, given keys ~a" (symdict-keys bs) (symdict-keys as))))
     (map (lambda (k)
            (constraint:subtype (symdict-ref as k) (symdict-ref bs k)))
          (symdict-keys as)))
    ((constraint:subtype (type:app (type:name f1) a1s) (type:app (type:name f2) a2s))
     (unless (eq? f1 f2) (error 'subtype (format "type mismatch, expected ~s, given ~s" f2 f1)))
     (def v1s (type-name-variances f1))
     (unless (= (length v1s) (length a1s) (length a2s))
       (error 'subtype "wrong number of arguments to type constructor"))
     (flatten1 (map constraints-from-variance-pair v1s a1s a2s)))
    ((constraint:subtype (type:arrow a1s b1) (type:arrow a2s b2))
     (unless (length=? a1s a2s)
       (error 'subype "type mismatch, wrong number of arguments"))
     (append (map make-constraint:subtype a2s a1s)
             [(constraint:subtype b1 b2)]))
    ((constraint:subtype (type:var _) _)
     (error 'sub-constraints "internal error, expected a non-atomic constraint"))
    ((constraint:subtype _ (type:var _))
     (error 'sub-constraints "internal error, expected a non-atomic constraint"))
    (_
     (error 'sub-constraints (format "unknown constraint: ~r" c)))))

;; biunify : Constraints TyvarBisubst -> TyvarBisubst
(def (biunify cs tybi)
  (match cs
    ([] tybi)
    ([fst . rst]
     (match fst
       ((constraint:subtype (ptype:union _) _)
        (biunify (append (sub-constraints fst) rst) tybi))
       ((constraint:subtype _ (ntype:intersection _))
        (biunify (append (sub-constraints fst) rst) tybi))
       ((constraint:subtype (type:var a) (type:var b))
        (cond
          ((eq? a b) (biunify rst tybi))
          ; a does not occur in b
          ((symdict-has-key? tybi a)
           (with (((type-interval tn tp) (symdict-ref tybi a)))
             (def ati (type-interval (type-meet tn (type:var b)) tp))
             (def tybi2 (symdict (a ati)))
             (biunify (constraints-bisubst tybi2 rst) (symdict-put tybi a ati))))
          (else
           (let ()
             (def ati (type-interval (type-meet (type:var a) (type:var b)) (type:var a)))
             (def tybi2 (symdict (a ati)))
             (biunify (constraints-bisubst tybi2 rst) (symdict-put tybi a ati))))))
       ((constraint:subtype (type:var a) b)
        (cond
          ((type-has-var? b a) (error 'subtype "recursive types are not yet supported"))
          ; a does not occur in b
          ((symdict-has-key? tybi a)
           (with (((type-interval tn tp) (symdict-ref tybi a)))
             (def ati (type-interval (type-meet tn b) tp))
             (def tybi2 (symdict (a ati)))
             (biunify (constraints-bisubst tybi2 rst) (symdict-put tybi a ati))))
          (else
           (let ()
             (def ati (type-interval (type-meet (type:var a) b) (type:var a)))
             (def tybi2 (symdict (a ati)))
             (biunify (constraints-bisubst tybi2 rst) (symdict-put tybi a ati))))))
       ((constraint:subtype a (type:var b))
        (cond
          ((type-has-var? a b) (error 'subtype "recursive types are not yet supported"))
          ; b does not occur in a
          ((symdict-has-key? tybi b)
           (with (((type-interval tn tp) (symdict-ref tybi b)))
             (def bti (type-interval tn (type-join tp a)))
             (def tybi2 (symdict (b bti)))
             (biunify (constraints-bisubst tybi2 rst) (symdict-put tybi b bti))))
          (else
           (let ()
             (def bti (type-interval (type:var b) (type-join (type:var b) a)))
             (def tybi2 (symdict (b bti)))
             (biunify (constraints-bisubst tybi2 rst) (symdict-put tybi b bti))))))
       (_
        (biunify (append (sub-constraints fst) rst) tybi))))))

; literals:
;   common:
;     \@
;   stmt:
;     : quote def λ deftype defdata publish!
;   expr/pat:
;     : ann \@tuple \@record \@list \@or-pat if block switch _ require! assert! deposit! withdraw!
;   type:
;     quote \@tuple \@record

;; parse-type : MPart Env Variance TyvarBisubst TypeStx -> Type
;; The variance should only be used to determine which entry from the
;; bisubstitution should be used when looking up a variable.
(def (parse-type part env vnc xsbi stx)
  ;; party : TypeStx -> Type
  ;; Doesn't change variance, only suitable for covariant s relative to stx
  (def (party s) (parse-type part env vnc xsbi s))
  (def t
   (syntax-case stx (@ quote @tuple @record)
     ((@ _ _) (error 'parse-type "TODO: deal with @"))
     ((@tuple t ...) (type:tuple (stx-map party #'(t ...))))
     ((@record (fld t) ...)
      (type:record
       (list->symdict
        (stx-map (lambda (fld t) (cons (syntax-e fld) (party t)))
                 #'(fld ...)
                 #'(t ...)))))
     ('x (identifier? #'x)
      (let ((s (syntax-e #'x)))
        (cond
          ((symdict-has-key? xsbi s)
          ;; use variance to see which one
           (tyvar-bisubst (symdict-ref xsbi s) vnc s))
          (else (type:var s)))))
     (x (identifier? #'x)
      (let ((s (syntax-e #'x)))
        (unless (symdict-has-key? env s)
          (error s "unknown type"))
        (def ent (symdict-ref env s))
        (unless (mpart-can-use? part (entry-part ent))
          (error s "access allowed only for" (entry-part ent)))
        (match ent
          ((entry:type part* [] t)  t)
          ((entry:type _ [_ . _] _) (error s "expects type arguments"))
          (_                        (error s "not a type")))))
     ((f a ...) (identifier? #'f)
      (let ((s (syntax-e #'f)))
        (unless (symdict-has-key? env s)
          (error s "unknown type"))
        (def ent (symdict-ref env s))
        (unless (mpart-can-use? part (entry-part ent))
          (error s "access allowed only for" (entry-part ent)))
        (match ent
          ((entry:type _ xvps b)
           (unless (= (length xvps) (stx-length #'(a ...)))
             (error 'parse-type "wrong number of type arguments"))
           (def tyvars2
             (for/fold (tyvars empty-symdict) ((xvp xvps) (a (syntax->list #'(a ...))))
               (with (([xn xp] xvp))
                 (symdict-put/list
                  tyvars
                  (append
                   (if xn [(cons xn (parse-type part env (variance-flip vnc) xsbi a))] [])
                   (if xp [(cons xp (parse-type part env vnc xsbi a))]))))))
           (type-subst tyvars2 b))
          (_ (error s "not a type")))))))
  (set-is-type stx t)
  t)

;; parse-closed-type : MPart Env TypeStx -> Type
;; When the tyvar bisubstitution is empty,
;; the variance doesn't matter and can be covariant
(def (parse-closed-type part env stx)
  (parse-type part env covariant empty-symdict stx))

;; parse-param-name : ParamStx -> Symbol
(def (parse-param-name p)
  (syntax-e (if (identifier? p) p (stx-car p))))

;; parse-param-type : MPart Env ParamStx -> (U #f Type)
(def (parse-param-type part env p)
  (syntax-case p (:)
    (x (identifier? #'x) #f)
    ((x : type) (parse-closed-type part env #'type))))

;; The reformulated rules produce judgements of the form
;; Π ⊩ e : [∆]τ
;; Function signatures follow: Env ExprStx -> TypingScheme

(def type:var_eq_a (type:var (gensym 'eq_a)))

;; init-env : Env
(def init-env
  (symdict
   ('Unit (entry:type #f [] type:Unit))
   ('Int (entry:type #f [] type:Int))
   ('Nat (entry:type #f [] type:Nat))
   ('Bool (entry:type #f [] type:Bool))
   ('Bytes (entry:type #f [] type:Bytes))
   ('Participant (entry:type #f [] type:Participant))
   ('Digest (entry:type #f [] type:Digest))
   ('Assets (entry:type #f [] type:Assets))
   ('Signature (entry:type #f [] type:Signature))
   ('not (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Bool] type:Bool))))
   ('<= (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Bool))))
   ('< (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Bool))))
   ('> (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Bool))))
   ('>= (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Bool))))
   ('+ (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Int))))
   ('- (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Int))))
   ('* (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Int))))
   ('/ (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Int))))
   ('mod (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Nat] type:Nat))))
   ('bitwise-not (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int] type:Int))))
   ('bitwise-and (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Int))))
   ('bitwise-or (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Int))))
   ('bitwise-xor (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Int))))
   ('bitwise-shift-right (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Int))))
   ('bitwise-shift-left (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int type:Int] type:Int))))
   ;; TODO: make polymorphic
   ('member (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Int (type:listof type:Int)] type:Bool))))
   ('randomUInt256 (entry:known #f (typing-scheme empty-symdict (type:arrow [] type:Nat))))
   ('isValidSignature (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Participant type:Digest type:Signature] type:Bool))))))

;; typecheck : ModuleStx UnusedTable → (values Env TypeInfoTable)
;; Input unused-table is mutated.
;; Output env has types of top-level identifiers.
(def (typecheck module unused-table)
  (def type-info-tbl (copy-current-type-info-table))
  (parameterize ((current-unused-table unused-table)
                 (current-type-info-table type-info-tbl)
                 (current-symbol-ntype-table (make-symbol-ntype-table)))
    (syntax-case module (@module)
      ((@module stmts ...)
       (let-values (((penv nenv) (tc-stmts #f init-env (syntax->list #'(stmts ...)))))
         (unless (symdict-empty? nenv)
           (error 'typecheck "non-empty D⁻ for free lambda-bound vars at top level"))
         (values penv type-info-tbl))))))

;; tc-stmts : MPart Env [Listof StmtStx] -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statements
(def (tc-stmts part env stmts)
  (let loop ((env env) (stmts stmts) (accpenv empty-symdict) (accnenv empty-symdict))
    (match stmts
      ([] (values accpenv accnenv))
      ([stmt . rst]
       (let-values (((penv nenv) (tc-stmt part env stmt)))
         (loop (env-put/env env penv) rst (env-put/env accpenv penv) (menv-meet accnenv nenv)))))))

;; tc-body : MPart Env BodyStx -> TypingScheme
(def (tc-body part env stx)
  (cond ((stx-null? stx) (typing-scheme empty-symdict type:Unit))
        ((stx-null? (stx-cdr stx)) (tc-expr/stmt part env (stx-car stx)))
        (else
         (let-values (((penv nenv) (tc-stmt part env (stx-car stx))))
           (typing-scheme/menv nenv (tc-body part (env-put/env env penv) (stx-cdr stx)))))))

;; tc-body/check : MPart Env BodyStx (U #f Type) -> TypingScheme
(def (tc-body/check part env stx expected-ty)
  (cond ((stx-null? stx)
         (unless (or (not expected-ty) (subtype? type:Unit expected-ty))
           (error 'tc-body/check "type mismatch with implicit unit at end of body"))
         (typing-scheme empty-symdict (or expected-ty type:Unit)))
        ((stx-null? (stx-cdr stx)) (tc-expr/stmt/check part env (stx-car stx) expected-ty))
        (else
         (let-values (((penv nenv) (tc-stmt part env (stx-car stx))))
           (typing-scheme/menv nenv (tc-body/check part (env-put/env env penv) (stx-cdr stx) expected-ty))))))

;; tc-stmt : MPart Env StmtStx -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt part env stx)
  (syntax-case stx (@ @debug-label @interaction : quote splice def λ deftype defdata publish!)
    ((@debug-label . _) (values empty-symdict empty-symdict))
    ((@ p _) (identifier? #'p) (tc-stmt-at-participant part env stx))
    ((splice s ...) (tc-stmts part env (syntax->list #'(s ...))))
    ((deftype . _) (tc-stmt-deftype part env stx))
    ((defdata . _) (tc-stmt-defdata part env stx))
    ((def . _) (tc-stmt-def part env stx))
    ((publish! participant v) (and (identifier? #'participant) (identifier? #'v))
     (tc-stmt-publish part env stx))
    (expr
     (with (((typing-scheme menv _) (tc-expr part env #'expr)))
       (values empty-symdict menv)))))

;; tc-stmt-at-participant : MPart Env StmtStx -> (values Env MonoEnv)
(def (tc-stmt-at-participant part env stx)
  (when part
    (error 'at-participant "already within a participant"))
  (syntax-case stx (@)
    ((@ p s)
     (let ((part* (syntax-e #'p))
           (ts (tc-expr/check part env #'p type:Participant)))
       (defvalues (penv2 nenv2) (tc-stmt part* env #'s))
       (values penv2 (menv-meet (typing-scheme-menv ts) nenv2))))))

;; tc-stmt-deftype : MPart Env StmtStx -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt-deftype part env stx)
  (syntax-case stx (@ : quote deftype defdata)
    ((deftype x t) (identifier? #'x)
     (values
      (symdict
       ((syntax-e #'x)
        (entry:type part [] (parse-closed-type part env #'t))))
      empty-symdict))
    ((deftype (f 'x ...) b) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map syntax-e #'(x ...))))
       ;; xvps : [Listof ParamVariancePair]
       (def xvps
         (for/collect ((x xs))
           [(symbol-fresh (format-symbol "~aₙ" x))
            (symbol-fresh (format-symbol "~aₚ" x))]))
       ;; xsbi : TyvarBisubst
       (def xsbi
         (for/fold (acc empty-symdict) ((x xs) (xvp xvps))
           (with (([xn xp] xvp))
             (symdict-put acc x (type-interval (type:var xn) (type:var xp))))))
       ;; bt : Type
       (def bt (parse-type part env covariant xsbi #'b))
       ;; TODO: infer variance by looking at which xn/xp versions are used in bt,
       ;;       and then update xvps to drop the unused ones
       (values
        (symdict
         (s
          (entry:type part xvps bt)))
        empty-symdict)))))

;; tc-stmt-defdata : MPart Env StmtStx -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt-defdata part env stx)
  (syntax-case stx (@ : quote deftype defdata)
    ((defdata x variant ... with: rtvalue) (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (def sym (symbol-fresh s))
       (def b (type:name sym))
       (set-is-type #'x b)
       (def env2 (symdict (s (entry:type part [] b))))
       ;; TODO: this pattern would be easier with splicing-parameterize
       (defvalues (penv nenv methods)
         (parameterize ((current-type-info-table (copy-current-type-info-table)))
           (add-type-info! sym (type-info [] empty-methods))
           (defvalues (penv nenv)
            (tc-defdata-variants part env env2 empty-symdict b #'(variant ...)))
           ;; handle the rtvalue
           (def methods (tc-expr part (env-put/envs env [env2 penv]) #'rtvalue))
           (values penv nenv methods)))
       (add-type-info! sym (type-info [] methods))
       (values (env-put/env env2 penv) nenv)))
    ((defdata (f 'x ...) variant ... with: rtvalue) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map syntax-e #'(x ...))))
       (def sym (symbol-fresh s))
       ;; xvps : [Listof ParamVariancePair]
       (def xvps
         (for/collect ((x xs))
           [(symbol-fresh (format-symbol "~aₙ" x))
            (symbol-fresh (format-symbol "~aₚ" x))]))
       ;; xsbi : TyvarBisubst
       (def xsbi
         (for/fold (acc empty-symdict) ((x xs) (xvp xvps))
           (with (([xn xp] xvp))
             (symdict-put acc x (type-interval (type:var xn) (type:var xp))))))
       ;; TODO: allow variances to be either annotated or inferred
       (def vances (map (lambda (x) invariant) xs))
       (def ft (type:name sym))
       (set-is-type #'f ft)
       (def b
         (type:app ft
                   (for/collect ((xvp xvps))
                     (with (([xn xp] xvp)) [(type:var xn) (type:var xp)]))))
       (def env2 (symdict (s (entry:type part xvps b))))
       ;; TODO: this pattern would be easier with splicing-parameterize
       (defvalues (penv nenv methods)
         (parameterize ((current-type-info-table (copy-current-type-info-table)))
           (add-type-info! sym (type-info vances empty-methods))
           (defvalues (penv nenv)
             (tc-defdata-variants part env env2 xsbi b #'(variant ...)))
           ;; handle the rtvalue
           (def methods (tc-expr part (env-put/envs env [env2 penv]) #'rtvalue))
           (values penv nenv methods)))
       (add-type-info! sym (type-info vances methods))
       (values (env-put/env env2 penv) nenv)))))


;; tc-defdata-variant : MPart Env TyvarBisubst Type VariantStx -> Env
;; the env result contains only the new symbols introduced by the variant
(def (tc-defdata-variant part env xsbi b stx)
  ;; party : TypeStx -> Type
  (def (party s) (parse-type part env covariant xsbi s))
  (syntax-case stx ()
    (x (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (symdict (s (entry:ctor part (typing-scheme empty-symdict b))))))
    ((f a ...) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (t (type:arrow (stx-map party #'(a ...)) b)))
       (symdict (s (entry:ctor part (typing-scheme empty-symdict t))))))))

;; tc-defdata-variants : MPart Env Env TyvarBisubst Type [StxListof VariantStx] -> (values Env MonoEnv)
;; the env result contains only the new symbols introduced by the variants
(def (tc-defdata-variants part env1 env2 xsbi b stx)
  (def env12 (env-put/env env1 env2))
  ;; tcvariant : VariantStx -> Env
  (def (tcvariant v) (tc-defdata-variant part env12 xsbi b v))
  (values
   (env-put/envs empty-symdict (stx-map tcvariant stx))
   empty-symdict))

(def (tx-optional-type-ann part env stx)
  (syntax-case stx (:)
    ((: type) (parse-type part env covariant empty-symdict #'type))
    (_ #f)))

;; tc-stmt-def : MPart Env StmtStx -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt-def part env stx)
  (syntax-case stx (@ : quote def λ)
    ((def f () (λ params ?out-type body ...)) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map parse-param-name #'params))
           (in-ts (stx-map (lambda (p) (parse-param-type part env p)) #'params))
           (out-t (tx-optional-type-ann part env #'?out-type)))
       (def fn-stx (stx-list-ref stx 3))
       (def fn-ts (tc-function part env xs in-ts out-t #'(body ...)))
       (set-has-typing-scheme fn-stx fn-ts)
       (tc-stmt-def/typing-scheme part env s fn-ts)))
    ((def x ?type expr) (identifier? #'x)
     (let ((s (syntax-e #'x))
           (t (tx-optional-type-ann part env #'?type)))
       (tc-stmt-def/typing-scheme
        part env s (if t (tc-expr/check part env #'expr t) (tc-expr part env #'expr)))))))

;; tc-stmt-def/typing-scheme : MPart Env Symbol TypingScheme -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt-def/typing-scheme part env s ts)
  (values
   (symdict (s (entry:known part ts)))
   (typing-scheme-menv ts)))

;; tc-expr-lambda : MPart Env ExprStx -> TypingScheme
(def (tc-expr-lambda part env stx)
  (syntax-case stx (λ)
    ((λ params ?out-type body ...)
     (let ((xs (stx-map parse-param-name #'params))
           (in-ts (stx-map (lambda (p) (parse-param-type part env p)) #'params))
           (out-t (syntax-case #'?out-type (:)
                    ((: out-type) (parse-type part env covariant empty-symdict #'out-type))
                    (_ #f))))
       (tc-function part env xs in-ts out-t #'(body ...))))))

;; tc-function : MPart Env [Listof Symbol] [Listof (U #f Type)] (U #f Type) BodyStx -> TypingScheme
(def (tc-function part env xs in-tys exp-out-ty body)
  ;; in-ents* : [Listof EnvEntry]
  ;; entry:unknown for parameters that weren't annotated
  (def in-ents*
    (map (lambda (in-ty)
           (cond (in-ty (entry:known part (typing-scheme empty-symdict in-ty)))
                 (else  (entry:unknown part))))
         in-tys))
  ;; body-env : Env
  (def body-env (symdict-put/list env (map cons xs in-ents*)))
  (with (((typing-scheme body-menv body-ty) (tc-body/check part body-env body exp-out-ty)))
    ;; in-tys* : [Listof NType]
    (def in-tys*
      (map (lambda (x in-ty)
             (or in-ty (symdict-get body-menv x ntype:top)))
           xs
           in-tys))
    ;; remove the xs from the menv before passing it up
    (def menv (for/fold (acc body-menv) ((x xs) (in-ty in-tys))
                (cond (in-ty acc)
                      (else (symdict-remove acc x)))))
    (typing-scheme menv (type:arrow in-tys* body-ty))))

;; tc-expr-make-interaction : MPart Env ExprStx -> TypingScheme
;; the env result contains only the new entries introduced by the statement
;; TODO: wrap the function in an interaction constructor (that remembers the number of participants?)
(def (tc-expr-make-interaction part env stx)
  (when part
    (error 'interaction "not allowed within a specific participant"))
  (syntax-case stx (@make-interaction @record participants assets @list)
    ;; TODO: when we design a separate way of calling interactions, create a separate
    ;;       type for interaction functions that forces them to be called specially
    ;;       as interactions, not just functions with extra participant arguments
    ((@make-interaction ((@record (participants (@list p ...)) (assets (@list a ...))))
                        params
                        ?out-type
                        . body)
     (stx-andmap identifier? #'(p ... a ...))
     (tc-expr part env #'(λ ((p : Participant) ... . params) ?out-type . body)))))

;; tc-stmt-publish : MPart Env StmtStx -> (values Env MonoEnv)
(def (tc-stmt-publish part env stx)
  (syntax-case stx (publish!)
    ((publish! participant v) (and (identifier? #'participant) (identifier? #'v))
     (if part (error 'publish! "only allowed in the consensus")
         (let (pt (tc-expr/check part env #'participant type:Participant))
           ;; TODO LATER: check that v is a variable with a *data* type.
           (values (env-publish env (stx-e #'v)) (typing-scheme-menv pt)))))))

;; tc-expr/stmt : MPart  Env StmtStx -> TypingScheme
(def (tc-expr/stmt part env stx)
  (def ts (tc-expr* part env stx))
  (cond (ts   (*as-expr stx ts))
        (else (let-values (((p n) (tc-stmt part env stx)))
                (typing-scheme n type:Unit)))))

;; tc-expr : MPart Env ExprStx -> TypingScheme
(def (tc-expr part env stx)
  (*as-expr stx (tc-expr* part env stx)))

;; *as-expr : ExprStx [Maybe TypingScheme] -> TypingScheme
(def (*as-expr stx ts)
  (unless ts (raise-syntax-error #f "expected an expression" stx))
  (set-has-typing-scheme stx ts)
  ts)

;; tc-expr* : MPart Env ExprStx -> [Maybe TypingScheme]
(def (tc-expr* part env stx)
  ;; tce : ExprStx -> TypingScheme
  (def (tce e) (tc-expr part env e))
  ;; tce/bool : ExprStx -> TypingScheme
  (def (tce/bool e) (tc-expr/check part env e type:Bool))
  (syntax-case stx (: ann @dot @dot/type @tuple @record @list @app-ctor @app and or if block splice == sign λ @make-interaction switch input digest require! assert! deposit! withdraw!)
    ((ann expr type)
     (tc-expr/check part env #'expr (parse-type part env covariant empty-symdict #'type)))
    (x (identifier? #'x) (tc-expr-id part env stx))
    (lit (stx-atomic-literal? #'lit) (typing-scheme empty-symdict (tc-literal #'lit)))
    ((@dot e x) (identifier? #'x) (tc-expr-dot (tce #'e) (syntax-e #'x)))
    ((@dot/type t x) (identifier? #'x) (tc-expr-dot (tc-type-methods part env #'t) (syntax-e #'x)))
    ((@tuple e ...)
     (let ((ts (stx-map tce #'(e ...))))
       (typing-scheme (menvs-meet (map typing-scheme-menv ts))
                      (type:tuple (map typing-scheme-type ts)))))
    ((@record (x e) ...)
     (and (stx-andmap identifier? #'(x ...))
          (check-duplicate-identifiers #'(x ...)))
     (let ((s (stx-map syntax-e #'(x ...)))
           (ts (stx-map tce #'(e ...))))
       (typing-scheme
        (menvs-meet (map typing-scheme-menv ts))
        (type:record
         (list->symdict (map cons s (map typing-scheme-type ts)))))))
    ((@list e ...)
     (let ((ts (stx-map tce #'(e ...))))
       (typing-scheme
        (menvs-meet (map typing-scheme-menv ts))
        (type:listof (types-join (map typing-scheme-type ts))))))
    ((block b ...)
     (tc-body part env #'(b ...)))
    ((splice b ...)
     (tc-body part env #'(b ...)))
    ((== a b)
     (let ((at (tce #'a)) (bt (tce #'b)))
       ;; TODO: constrain the types in `at` and `bt` to types that
       ;;       can be compared for equality
       (typing-scheme (menvs-meet (map typing-scheme-menv [at bt]))
                      type:Bool)))
    ((sign e)
     (let ((ts (tc-expr/check part env #'e type:Digest)))
       (typing-scheme (typing-scheme-menv ts) type:Signature)))
    ((λ . _) (tc-expr-lambda part env stx))
    ((@make-interaction . _) (tc-expr-make-interaction part env stx))
    ((switch e swcase ...)
     (let ((ts (tc-expr part env #'e)))
       (tc-switch-cases part env ts (syntax->list #'(swcase ...)))))
    ((input type tag)
     (let ((t (parse-type part env covariant empty-symdict #'type))
           (ts (tc-expr/check part env #'tag type:Bytes)))
       (typing-scheme (typing-scheme-menv ts) t)))
    ((digest e ...)
     (let ((ts (stx-map tce #'(e ...))))
       ;; TODO: constrain the types in `ts` to types that digests
       ;;       can be computed for
       (typing-scheme (menvs-meet (map typing-scheme-menv ts))
                      type:Digest)))
    ((require! e)
     (let ((et (tce/bool #'e)))
       (typing-scheme (typing-scheme-menv et) type:Unit)))
    ((assert! e)
     (let ((et (tce/bool #'e)))
       (typing-scheme (typing-scheme-menv et) type:Unit)))
    ((deposit! x e) (identifier? #'x)
     (tc-deposit-withdraw part env stx))
    ((withdraw! x e) (identifier? #'x)
     (tc-deposit-withdraw part env stx))
    ((@app-ctor f a ...) (tc-expr-app part env stx))
    ((@app f a ...) (tc-expr-app part env stx))
    (_ #f)))

;; tc-expr-app : MPart Env Stx -> TypingScheme
(def (tc-expr-app part env stx)
  (syntax-case stx ()
    ((_ f a ...)
     (let ((s (syntax-e (head-id #'f)))
           (ft (tc-expr part env #'f)))
       (match (typing-scheme-type ft)
         ((type:arrow in-tys out-ty)
          (unless (= (stx-length #'(a ...)) (length in-tys))
            (error s "wrong number of arguments"))
          (def ats
            (stx-map (lambda (a t) (tc-expr/check part env a t))
                     #'(a ...)
                     in-tys))
          (typing-scheme (menvs-meet (cons (typing-scheme-menv ft) (map typing-scheme-menv ats)))
                         out-ty))
         ((type:var v)
          ;; fresh type variables
          (def avs (stx-map (lambda (a)
                              (cond ((identifier? a) (symbol-fresh (syntax-e a)))
                                    (else (symbol-fresh s))))
                            #'(a ...)))
          (def bv (symbol-fresh s))
          ;; unify against arrow
          ;; v <: (-> avs bv)
          (def fty (type:arrow (map make-type:var avs) (type:var bv)))
          (def atss (stx-map (lambda (a) (tc-expr part env a)) #'(a ...)))
          (def ats (map typing-scheme-type atss))
          (def bisub
            (biunify (cons (constraint:subtype (type:var v) fty)
                           (map (lambda (at av)
                                  (constraint:subtype at (type:var av)))
                                ats
                                avs))
                     empty-symdict))
          (typing-scheme-bisubst
           bisub
           (typing-scheme (menvs-meet (cons (typing-scheme-menv ft) (map typing-scheme-menv atss)))
                          (type:var bv))))
         (t (error s "expected a function, given" (typing-scheme->repr-sexpr ft))))))))

;; tc-expr-id : MPart Env Identifier -> TypingScheme
(def (tc-expr-id part env x)
  (def s (syntax-e x))
  (unless (symdict-has-key? env s)
    (error s "unbound identifier"))
  (def ent (symdict-ref env s))
  (unless (mpart-can-use? part (entry-part ent))
    (error s "access allowed only for" (entry-part ent)))
  (match ent
    ((entry:unknown _)
     (let ((a (symbol-fresh s)))
       (typing-scheme (list->symdict [(cons s (type:var a))])
                      (type:var a))))
    ((entry:known _ t) t)
    ((entry:ctor _ t) t)))

;; tc-expr-dot : TypingScheme Symbol -> TypingScheme
(def (tc-expr-dot ts s)
  (match (typing-scheme-type ts)
    ((type:record fldtys)
     (unless (symdict-has-key? fldtys s)
       (error '@dot "expected record with field" s))
     (typing-scheme (typing-scheme-menv ts)
                    (symdict-ref fldtys s)))
    (t
     (error 'tc-expr-dot "TODO: @dot, unify with a record type" "et" (syntax->datum #'et) "ts" ts "(typing-scheme? ts)" (typing-scheme? ts) "t" t))))

;; tc-type-methods : MPart Env TypeStx -> TypingScheme
;; Typechecks stx as a type with methods in a record that can be accessed with `@dot/type`
(def (tc-type-methods part env stx)
  (syntax-case stx (quote)
    (x (and (identifier? #'x) (bound-as-type? env (syntax-e #'x)))
     (let ((t (parse-closed-type part env #'x)))
       (type-methods t)))
    ('x (identifier? #'x)
     ;; TODO: when traits are added, look in the trait constraints
     (error 'tc-type-methods "type variable used out of context:" (syntax->datum stx)))
    ((tf . _) (and (identifier? #'x) (bound-as-type? env (syntax-e #'tf)))
     ;; TODO: when traits are added, handle trait constraints for arguments
     (error 'tc-type-methods "TODO: handle types with arguments used as records for @dot"))
    (_
     (error 'tc-type-methods "not a type with methods" (syntax->datum stx)))))

;; type-methods : Type -> TypingScheme
(def (type-methods t)
  (match t
    ((type:name name)
     (type-name-methods name))
    ((type:name-subtype name _) (type-name-methods name))
    (_ (error 'type-methods "only name types can have methods"))))

;; tc-expr/check : MPart Env ExprStx (U #f NType) -> TypingSchemeOrError
;; returns expected-ty on success, actual-ty if no expected, otherwise error
(def (tc-expr/check part env stx expected-ty)
  (unless (or (not expected-ty) (ntype? expected-ty))
    (error 'tc-expr/check "expected (U #f Type) for 4th argument" expected-ty))
  (def actual-ts (tc-expr part env stx))
  (typing-scheme/check actual-ts expected-ty))

;; typing-scheme/check : TypingScheme (U #f NType) -> TypingSchemeOrError
(def (typing-scheme/check actual-ts expected-ty)
  (cond
    ((not expected-ty) actual-ts)
    (else
     (with (((typing-scheme menv actual-ty) actual-ts))
       (def bity (biunify [(constraint:subtype actual-ty expected-ty)] empty-symdict))
       (def ts-before (typing-scheme menv expected-ty))
       (def ts-after (typing-scheme-bisubst bity ts-before))
       ts-after))))

;; tc-expr/stmt/check : MPart Env Expr (U #f NType) -> TypingSchemeOrError
(def (tc-expr/stmt/check part env stx expected-ty)
  (unless (or (not expected-ty) (ntype? expected-ty))
    (error 'tc-expr/check "expected (U #f Type) for 4th argument" expected-ty))
  (def actual-ts (tc-expr/stmt part env stx))
  (typing-scheme/check actual-ts expected-ty))

;; tc-deposit-withdraw : MPart Env ExprStx -> TypingScheme
(def (tc-deposit-withdraw part env stx)
  (syntax-case stx (@record)
    ((dw participant (@record (asset amount)))
     (let ()
       (when part (error (stx-e #'dw) "only allowed in the consensus"))
       (def pt (tc-expr/check part env #'participant type:Participant))
       (def at (tc-expr/check part env #'amount type:Int))
       (typing-scheme (menvs-meet (map typing-scheme-menv [pt at]))
                      type:Unit)))))

;; tc-literal : LiteralStx -> Type
;; Produces the most specific type for the literal
(def (tc-literal stx)
  (def e (stx-e stx))
  (cond ((exact-integer? e)
         (cond ((negative? e) type:Int)
               (else type:Nat))) ; Nat when non-negative, more specific
        ((boolean? e) type:Bool)
        ((string? e) type:Bytes) ; represent as bytess using UTF-8
        ((u8vector? e) type:Bytes)
        ((and (pair? e) (length=n? e 1) (equal? (stx-e (car e)) '@tuple)) type:Unit)
        (else (error 'tc-literal "unrecognized literal"))))

;; tc-pat-literal : LiteralStx -> Type
;; Produces the most permissive type for the literal, not the most specific
(def (tc-pat-literal stx)
  (def e (stx-e stx))
  (cond ((exact-integer? e) type:Int) ; never Nat, Int is more permissive
        ((boolean? e) type:Bool)
        ((string? e) type:Bytes)
        ((u8vector? e) type:Bytes)
        (else (error 'tc-pat-literal "unrecognized literal"))))

;; tc-switch-cases : MPart Env TypingScheme [Listof SwitchCaseStx] -> TypingScheme
(def (tc-switch-cases part env valts stxs)
  (with (((typing-scheme valnenv valty) valts)
         ([nenvs ntypes ptypes]
          (transpose/nlist 3 (map (cut tc-switch-case part env valts <>) stxs))))
    (def ntype (types-meet ntypes))
    (def ptype (types-join ptypes))
    (def nenv (menvs-meet (cons valnenv nenvs)))
    (def before-ts (typing-scheme nenv ptype))
    ;; valty <: ntype
    ;(printf "tc-switch-cases: ~r <: ~r\n" valty ntype)
    (def bity (biunify [(constraint:subtype valty ntype)] empty-symdict))
    (typing-scheme-bisubst bity before-ts)))

;; tc-switch-case : MPart Env TypingScheme SwitchCaseStx -> (list MonoEnv NType PType)
(def (tc-switch-case part env valts stx)
  (syntax-case stx ()
    ((pat body ...)
     (with (((typing-scheme valnenv valty) valts)
            ((pat-typing-scheme nenv1 ntype pattys)
             (tc-pat part env #'pat)))
       (def patrecordtype (type:record (list->symdict pattys)))
       (def xs (map car pattys))
       ;; each pattern-variable gets unknown
       (def env/patvars
         (symdict-put/list
          env
          (map (cut cons <> (entry:unknown part)) xs)))
       (with (((typing-scheme nenv2 ptype) (tc-body part env/patvars #'(body ...))))
         (def nenv3 (menvs-meet [valnenv nenv1 nenv2]))
         ;; unify pattys with their respective tys in nenv3
         (def bity (biunify (cons
                             (constraint:subtype valty ntype)
                             (for/collect ((e pattys))
                              (with (((cons x ty) e))
                                (constraint:subtype ty (symdict-get nenv3 x ntype:top)))))
                            empty-symdict))
         ;; remove the pattys before passing the final nenv up
         (def nenv (for/fold (acc nenv3) ((x xs))
                     (symdict-remove acc x)))
         ;; apply the bisubstitution to the final nenv, the ntype, and the ptype
         (def nenv* (menv-bisubst bity nenv))
         (def ntype* (ntype-bisubst bity ntype))
         (def ptype* (ptype-bisubst bity ptype))
         (def patrecordtype* (ptype-bisubst bity patrecordtype))
         (set-has-typing-scheme stx (typing-scheme nenv* (type:arrow [ntype*] (type:tuple [patrecordtype* ptype*]))))
         ;(for ((e pattys))
         ;  (with (((cons x ty) e))
         ;    (set-has-typing-scheme x (typing-scheme nenv* (ptype-bisubst bity ty)))))
         [nenv* ntype* ptype*])))))

;; tc-pat : MPart Env PatStx -> PatTypingScheme
;; Produces the most permissive type for the pattern
(def (tc-pat part env stx)
  (syntax-case stx (@ : ann @tuple @record @list @or-pat @var-pat @app-ctor)
    ((@ _ _) (error 'tc-pat "TODO: deal with @"))
    ((ann pat type)
     (with ((t (parse-type part env covariant empty-symdict #'type))
            ((pat-typing-scheme nenv ntype ptys) (tc-pat part env #'pat)))
       ;; t <: ntype
       (def bity (biunify [(constraint:subtype t ntype)] empty-symdict))
       (pat-typing-scheme-bisubst bity (pat-typing-scheme nenv t ptys))))
    (blank (and (identifier? #'blank) (free-identifier=? #'blank #'_))
     (pat-typing-scheme empty-symdict ntype:top []))
    ((@var-pat x) (identifier? #'x)
     ;; pattern variable, since it's not a constructor
     (let ((s (syntax-e #'x)))
       (def s2 (symbol-fresh s))
       (pat-typing-scheme empty-symdict (type:var s2) [(cons s (type:var s2))])))
    (lit (stx-atomic-literal? #'lit)
     (let ((t (tc-pat-literal #'lit))) ; t is the most permissive type for the literal
       (pat-typing-scheme empty-symdict t [])))
    ((@or-pat p ...)
     (pat-typing-schemes-join
      (stx-map (cut tc-pat part env <>) #'(p ...))))
    ((@list p ...)
     (let ((pts (stx-map (cut tc-pat part env <>) #'(p ...))))
       (def nenv (menvs-meet (map pat-typing-scheme-menv pts)))
       ;; meet because the ntypes are already as permissive as they can be
       ;; the type of the actual elements of the list, must be a subtype of all
       ;; the ntypes of the sub-patterns
       (def nty (type:listof (types-meet (map pat-typing-scheme-ntype pts))))
       (def ptys (pattys-append (map pat-typing-scheme-pattys pts)))
       (pat-typing-scheme nenv nty ptys)))
    ((@tuple p ...)
     (let ((pts (stx-map (cut tc-pat part env <>) #'(p ...))))
       (def nenv (menvs-meet (map pat-typing-scheme-menv pts)))
       (def nty (type:tuple (map pat-typing-scheme-ntype pts)))
       (def ptys (pattys-append (map pat-typing-scheme-pattys pts)))
       (pat-typing-scheme nenv nty ptys)))
    ((@record (x p) ...) (stx-andmap identifier? #'(x ...))
     (let ((s (stx-map syntax-e #'(x ...)))
           (pts (stx-map (cut tc-pat part env <>) #'(p ...))))
       (def nenv (menvs-meet (map pat-typing-scheme-menv pts)))
       (def ntys (map pat-typing-scheme-ntype pts))
       (def nty (type:record (list->symdict (map cons s ntys))))
       (def ptys (pattys-append (map pat-typing-scheme-pattys pts)))
       (pat-typing-scheme nenv nty ptys)))
    ((@app-ctor f a ...) (and (identifier? #'f) (bound-as-ctor? env (syntax-e #'f)))
     (let ((s (syntax-e #'f)))
       (unless (symdict-has-key? env s)
         (error s "unbound pattern constructor"))
       (def ent (symdict-ref env s))
       (unless (mpart-can-use? part (entry-part ent))
         (error s "access allowed only for" (entry-part ent)))
       (match ent
         ((entry:ctor _ (typing-scheme nenv1 (type:arrow in-tys out-ty)))
          (unless (= (stx-length #'(a ...)) (length in-tys))
            (error s "wrong number of arguments to data constructor"))
          (def pts (stx-map (cut tc-pat part env <>) #'(a ...)))
          (def nenv2 (menvs-meet (cons nenv1 (map pat-typing-scheme-menv pts))))
          (def ntys (map pat-typing-scheme-ntype pts))
          (def ptys (pattys-append (map pat-typing-scheme-pattys pts)))
          ;; [in-ty <: nty] ...
          (def bity (biunify (map make-constraint:subtype in-tys ntys) empty-symdict))
          (pat-typing-scheme-bisubst bity (pat-typing-scheme nenv2 out-ty ptys)))
         ((entry:ctor _ (typing-scheme nenv t))
          (unless (or (type:name? t) (type:app? t))
            (error s "expected either a named datatype or an arrow type:" t))
          (unless (stx-null? #'(a ...))
            (error s "expected no arguments to data constructor"))
          (pat-typing-scheme nenv t [])))))))

;; --------------------------------------------------------

;(trace! biunify)

(export #t
        (import:
         "../common.ss"
         "variance.ss"
         "type.ss"))

(import :std/format
        :std/iter
        :std/misc/list
        :std/misc/repr
        :gerbil/gambit/exact
        :gerbil/gambit/bytes
        <expander-runtime>
        :glow/compiler/syntax-context
        (for-template :glow/compiler/syntax-context)
        :clan/pure/dict/assq
        :clan/pure/dict/symdict
        "../common.ss"
        (except-in "../alpha-convert/alpha-convert.ss" env-put/env not-bound-as-ctor? bound-as-ctor?)
        "variance.ss"
        "type.ss")

;; Typechecking glow-sexpr

;; Type system and scoping rules are closest
;; to ReasonML, but Record types are structural.

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
;; (~? irrelavent a b) = #t
;; (~? covariant a b) = (subtype? a b)
;; (~? contravariant a b) = (subtype? b a)
;; (~? invariant a b) = (type=? a b)
(def (variance-type~? v a b)
  (and (or (variance-covariant? v) (subtype? b a))
       (or (variance-contravariant? v) (subtype? a b))))

;; subtype? : PType NType -> Bool
(def (subtype? a b)
  (match* (a b)
    (((type:bottom) _) #t)
    (((ptype:union as) b)
     (andmap (lambda (a) (subtype? a b)) as))
    ((a (ntype:intersection bs))
     (andmap (lambda (b) (subtype? a b)) bs))
    (((type:name x vxs) (type:name y vys))
     (and (eq? x y) (equal? vxs vys)))
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
    (((type:app (type:name f1 v1s) a1s) (type:app (type:name f2 v2s) a2s))
     (and (eq? f1 f2)
          (equal? v1s v2s)
          (= (length v1s) (length a1s) (length a2s))
          (andmap variance-type~? v1s a1s a2s)))
    ((_ _) #f)))

;; A Pattys is an [Assqof Symbol Type]
;; for the types of the pattern variables within a pattern

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

;; A MonoEnv is a [Symdictof NType]
;; monotype environments ∆ bind λ-bound variables only,
;; no known-type/let-bound variables, don't have ∀-quantifiers

;; menv-meet : MonoEnv MonoEnv -> MonoEnv
;; union of keys, type-meet of values
(def (menv-meet as bs)
  (for/fold (acc as) ((k (symdict-keys bs)))
    (def b (symdict-ref bs k))
    (cond
      ((symdict-has-key? acc k) (symdict-update acc k (lambda (a) (type-meet a b)) b))
      (else                     (symdict-put acc k b)))))

;; menvs-meet : [Listof MonoEnv] -> MonoEnv
;; union of keys, type-meet of values
(def (menvs-meet menvs)
  (for/fold (acc empty-symdict) ((me menvs)) (menv-meet acc me)))

;; A TypingScheme is a (typing-scheme MonoEnv PType)
;; Representing both the type of an expression and the types of its
;; free λ-bound variables. A polar typing scheme [D⁻]t⁺ is a typing
;; scheme where the types D⁻(x) of λ-bound variables are given by
;; negative type terms, and the type of the result t⁺ is given by a
;; positive type term.
(defstruct typing-scheme (menv type) transparent: #t)

(def (print-typing-scheme ts)
  (with (((typing-scheme menv t) ts))
    (display-separated
     (symdict-keys menv)
     prefix: "["
     suffix: "]"
     separator: ", "
     display-element: (lambda (k out)
                        (fprintf out "~a: ~s" k (type->sexpr (symdict-ref menv k)))))
    (write (type->sexpr t))))

;; typing-schemes-join : [Listof TypingScheme] -> TypingScheme
;; meet on menvs, join on types
(def (typing-schemes-join ts)
  (typing-scheme (menvs-meet (map typing-scheme-menv ts))
                 (types-join (map typing-scheme-type ts))))

;; typing-scheme/menv : MonoEnv TypingScheme -> TypingScheme
(def (typing-scheme/menv menv1 ts)
  (with (((typing-scheme menv2 t) ts))
    (typing-scheme (menv-meet menv1 menv2) t)))

;; An MPart is one of:
;;  - #f      ; consensus, or no participant
;;  - Symbol  ; within the named participant

;; mpart-can-use? : MPart MPart -> Bool
;; can code with `a` participant priviledges access `b`?
;; any participant can access `#f` consensus identifiers
;; participants can access their own private identifiers
;; the `#f` consensus cannot access participant-specific ids
(def (mpart-can-use? a b) (or (not b) (eq? a b)))

;; An EnvEntry is one of:
;;  - (entry:unknown MPart)             ; lambda-bound with no type annotation
;;  - (entry:known MPart TypingScheme)  ; let-bound, or lambda-bound with annotation
;;  - (entry:ctor MPart TypingScheme)   ; note ctor does not have N or P, just Type
;;  - (entry:type MPart [Listof Symbol] Type)
(defstruct entry:unknown (part) transparent: #t)
(defstruct entry:known (part ts) transparent: #t)
(defstruct entry:ctor (part ts) transparent: #t)
(defstruct entry:type (part params type) transparent: #t)
(def (env-entry? v) (or (entry:unknown? v) (entry:known? v) (entry:ctor? v) (entry:type? v)))

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
    ((entry:type _ xs t) (entry:type #f xs t))))

;; env-publish : Env [Listof Symbol] -> Env
;; Produces a new env containing only the published versions of the xs
(def (env-publish env xs)
  (list->symdict
   (for/collect ((x xs))
     (unless (symdict-has-key? env x) (error x "unbound identifier"))
     (cons x (entry-publish (symdict-ref env x))))))

;; An Env is a [Symdictof EnvEntry]
;; Instead of type environments Γ, the reformulated rules use typing
;; environments Π to assign typing schemes (not type schemes) to
;; known-type/let-bound variables.

;; env-put/env : Env Env -> Env
;; entries in the 2nd env override ones in the 1st
(def (env-put/env e1 e2) (symdict-put/list e1 (symdict->list e2)))

;; env-put/envs : Env [Listof Env] -> Env
;; entries later in the list override earlier ones
(def (env-put/envs e1 es2) (for/fold (e e1) (e2 es2) (env-put/env e e2)))

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

;; tyvar-bisubst : TypeInterval Variance Symbol -> PNType
(def (tyvar-bisubst tvl v s)
  (cond ((variance-covariant? v) (type-interval-ptype tvl))
        ((variance-contravariant? v) (type-interval-ntype tvl))
        ((eq? (type-interval-ptype tvl) (type-interval-ntype tvl))
         (type-interval-ptype tvl))
        (else (error s "could not infer type"))))

;; ptype-bisubst : TyvarBisubst Variance PNType -> PNType
;; P vs N depends on the variance
(def (type-bisubst tybi v t)
  ;; sub : PNType -> PNType
  (def (sub pt) (type-bisubst tybi v pt))
  ;; vsub : Variance PNType -> PNType
  ;; variance is within v, so must compose with v
  (def (vsub v2 t)
    (type-bisubst tybi (variance-compose v v2) t))
  (def (nsub t) (vsub contravariant t))
  (match t
    ((type:bottom) t)
    ((type:name _ _) t)
    ((type:var s)
     (cond ((symdict-has-key? tybi s)
            (tyvar-bisubst (symdict-ref tybi s) v s))
           (else t)))
    ((type:app (type:name f vs) as)
     (unless (= (length vs) (length as))
       (error f "wrong number of arguments to type constructor"))
     (type:app (type:name f vs) (map vsub vs as)))
    ((type:tuple as)
     (type:tuple (map sub as)))
    ((type:record fldtys)
     (type:record
      (list->symdict
       (map (lambda (p) (cons (car p) (sub (cdr p))))
            fldtys))))
    ((type:arrow as b)
     (type:arrow (map nsub as) (sub b)))
    ((ptype:union ts)
     (types-join (map sub ts)))
    ((ntype:intersection ts)
     (types-meet (map sub ts)))))

;; menv-bisubst : TyvarBisubst MonoEnv -> MonoEnv
(def (menv-bisubst tybi menv)
  (for/fold (acc empty-symdict) ((k (symdict-keys menv)))
    (symdict-put acc k (type-bisubst tybi contravariant (symdict-ref menv k)))))

;; typing-scheme-bisubst : TyvarBisubst TypingScheme -> TypingScheme
(def (typing-scheme-bisubst tybi ts)
  (with (((typing-scheme menv ty) ts))
    (typing-scheme (menv-bisubst tybi menv)
                   (type-bisubst tybi covariant ty))))

;; A Constraints is a [Listof Constraint]
;; A Constraint is one of:
;;  - (constraint:subtype PType NType)
;;  - (constraint:type-equal Type Type)
(defstruct constraint:subtype (a b) transparent: #t)
(defstruct constraint:type-equal (a b) transparent: #t)

;; constraints-from-variance : Variance PNType PNType -> Constraints
(def (constraints-from-variance v a b)
  (cond ((variance-irrelevant? v) [])
        ((variance-covariant? v) [(constraint:subtype a b)])
        ((variance-contravariant? v) [(constraint:subtype b a)])
        (else [(constraint:type-equal a b)])))

;; constraints-bisubst : TyvarBisubst Constraints -> Constraints
(def (constraints-bisubst tybi cs)
  (map (lambda (c) (constraint-bisubst tybi c)) cs))

;; constraint-bisubst : TyvarBisubst Constraint -> Constraint
(def (constraint-bisubst tybi c)
  (match c
    ((constraint:subtype a b)
     (constraint:subtype (type-bisubst tybi contravariant a)
                         (type-bisubst tybi covariant b)))
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
    ((constraint:subtype (type:bottom) _)
     [])
    ((constraint:subtype (ptype:union as) b)
     (map (lambda (a) (constraint:subtype a b)) as))
    ((constraint:subtype a (ntype:intersection bs))
     (map (lambda (b) (constraint:subtype a b)) bs))
    ((constraint:subtype (type:name x vxs) (type:name y vys))
     (unless (eq? x y) (error 'subtype (format "type mismatch, expected ~s, given ~s" y x)))
     (unless (equal? vxs vys) (error 'subtype "inconsistant variances"))
     [])
    ((constraint:subtype (type:tuple as) (type:tuple bs))
     (unless (= (length as) (length bs))
       (error 'subtype "tuple length mismatch"))
     (map make-constraint:subtype as bs))
    ((constraint:subtype (type:record as) (type:record bs))
     (unless (symdict-key-set=? as bs)
       (error 'subtype "record field mismatch"))
     (map (lambda (k)
            (constraint:subtype (symdict-ref as k) (symdict-ref bs k)))
          (symdict-keys as)))
    ((constraint:subtype (type:app (type:name f1 v1s) a1s) (type:app (type:name f2 v2s) a2s))
     (unless (eq? f1 f2) (error 'subtype (format "type mismatch, expected ~s, given ~s" f2 f1)))
     (unless (equal? v1s v2s) (error 'subtype "inconsistant variances"))
     (unless (= (length v1s) (length a1s) (length a2s))
       (error 'subtype "wrong number of arguments to type constructor"))
     (flatten1 (map constraints-from-variance v1s a1s a2s)))
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
       ((constraint:subtype (type:var a) (type:var b))
        (cond
          ((eq? a b) (biunify rst tybi))
          ; a does not occur in b
          ((symdict-has-key? tybi a)
           (with (((type-interval tn tp) (symdict-ref tybi a)))
             (def tybi2 (symdict-put tybi a (type-interval (type-meet tn (type:var b)) tp)))
             (biunify (constraints-bisubst tybi2 rst) tybi2)))
          (else
           (let ((tybi2 (symdict-put tybi a (type-interval (type-meet (type:var a) (type:var b)) (type:var a)))))
             (biunify (constraints-bisubst tybi2 rst) tybi2)))))
       ((constraint:subtype (type:var a) b)
        (cond
          ((type-has-var? b a) (error 'subtype "recursive types are not yet supported"))
          ; a does not occur in b
          ((symdict-has-key? tybi a)
           (with (((type-interval tn tp) (symdict-ref tybi a)))
             (def tybi2 (symdict-put tybi a (type-interval (type-meet tn b) tp)))
             (biunify (constraints-bisubst tybi2 rst) tybi2)))
          (else
           (let ((tybi2 (symdict-put tybi a (type-interval (type-meet (type:var a) b) (type:var a)))))
             (biunify (constraints-bisubst tybi2 rst) tybi2)))))
       ((constraint:subtype a (type:var b))
        (cond
          ((type-has-var? a b) (error 'subtype "recursive types are not yet supported"))
          ; b does not occur in a
          ((symdict-has-key? tybi b)
           (with (((type-interval tn tp) (symdict-ref tybi b)))
             (def tybi2 (symdict-put tybi b (type-interval tn (type-join tp a))))
             (biunify (constraints-bisubst tybi2 rst) tybi2)))
          (else
           (let ((tybi2 (symdict-put tybi b (type-interval (type:var b) (type-join (type:var b) a)))))
             (biunify (constraints-bisubst tybi2 rst) tybi2)))))
       (_
        (biunify (append (sub-constraints fst) rst) tybi))))))

; literals:
;   common:
;     \@
;   stmt:
;     : quote def λ deftype defdata publish! verify!
;   expr/pat:
;     : ann \@tuple \@record \@list \@or-pat if block switch _ require! assert! deposit! withdraw!
;   type:
;     quote \@tuple \@record

;; parse-type : MPart Env TyvarEnv TypeStx -> Type
(def (parse-type part env tyvars stx)
  ;; party : TypeStx -> Type
  (def (party s) (parse-type part env tyvars s))
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
       (cond ((symdict-has-key? tyvars s) (symdict-ref tyvars s))
             (else (type:var s)))))
    (x (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (unless (symdict-has-key? env s)
         (error s "unknown type"))
       (def ent (symdict-ref env s))
       (unless (mpart-can-use? part (entry-part ent))
         (error s "access allowed only for" (entry-part ent)))
       (match ent
         ((entry:type part* [] t)
          t))))
    ((f a ...) (identifier? #'f)
     (let ((s (syntax-e #'f)))
       (unless (symdict-has-key? env s)
         (error s "unknown type"))
       (def ent (symdict-ref env s))
       (unless (mpart-can-use? part (entry-part ent))
         (error s "access allowed only for" (entry-part ent)))
       (match ent
         ((entry:type _ xs b)
          (def as (stx-map party #'(a ...)))
          (unless (= (length xs) (length as))
            (error 'parse-type "wrong number of type arguments"))
          (def tyvars2 (symdict-put/list tyvars (map cons xs as)))
          (type-subst tyvars2 b)))))))

;; parse-param-name : ParamStx -> Symbol
(def (parse-param-name p)
  (syntax-e (if (identifier? p) p (stx-car p))))

;; parse-param-type : MPart Env ParamStx -> (U #f Type)
(def (parse-param-type part env p)
  (syntax-case p (:)
    (x (identifier? #'x) #f)
    ((x : type) (parse-type part env empty-symdict #'type))))

;; The reformulated rules produce judgements of the form
;; Π ⊩ e : [∆]τ
;; Function signatures follow: Env ExprStx -> TypingScheme

;; init-env : Env
(def init-env
  (symdict
   ('int (entry:type #f [] type:int))
   ('bool (entry:type #f [] type:bool))
   ('bytes (entry:type #f [] type:bytes))
   ('Participant (entry:type #f [] type:Participant))
   ('Digest (entry:type #f [] type:Digest))
   ('Assets (entry:type #f [] type:Assets))
   ('Signature (entry:type #f [] type:Signature))
   ('not (entry:known #f (typing-scheme empty-symdict (type:arrow [type:bool] type:bool))))
   ('== (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:bool))))
   ('= (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:bool))))
   ('<= (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:bool))))
   ('< (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:bool))))
   ('> (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:bool))))
   ('>= (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:bool))))
   ('+ (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:int))))
   ('- (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:int))))
   ('* (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:int))))
   ('/ (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:int))))
   ('mod (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int type:int] type:int))))
   ('sqr (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int] type:int))))
   ('sqrt (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int] type:int))))
   ;; TODO: make polymorphic
   ('member (entry:known #f (typing-scheme empty-symdict (type:arrow [type:int (type:listof type:int)] type:bool))))
   ('randomUInt256 (entry:known #f (typing-scheme empty-symdict (type:arrow [] type:int))))
   ('sign (entry:known #f (typing-scheme empty-symdict (type:arrow [type:Digest] type:Signature))))))

;; tc-prog : [Listof StmtStx] -> Env
(def (tc-prog stmts)
  (defvalues (stmts2 _unused-table acrenom) (alpha-convert stmts))
  (parameterize ((current-symbol-ntype-table (make-symbol-ntype-table)))
    (for/fold (env init-env) (stmt stmts2)
      (let-values (((penv nenv) (tc-stmt #f env stmt)))
        (unless (symdict-empty? nenv)
          (error 'tc-prog "non-empty D⁻ for free lambda-bound vars at top level"))
        (env-put/env env penv)))))

;; tc-body : MPart Env BodyStx -> TypingScheme
(def (tc-body part env stx)
  (cond ((stx-null? stx) (typing-scheme empty-symdict (type:tuple [])))
        ((stx-null? (stx-cdr stx)) (tc-expr part env (stx-car stx)))
        (else
         (let-values (((penv nenv) (tc-stmt part env (stx-car stx))))
           (typing-scheme/menv nenv (tc-body part (env-put/env env penv) (stx-cdr stx)))))))

;; tc-body/check : MPart Env BodyStx (U #f Type) -> TypingScheme
(def (tc-body/check part env stx expected-ty)
  (cond ((stx-null? stx)
         (unless (or (not expected-ty) (subtype? (type:tuple []) expected-ty))
           (error 'tc-body/check "type mismatch with implicit unit at end of body"))
         (typing-scheme empty-symdict (or expected-ty (type:tuple []))))
        ((stx-null? (stx-cdr stx)) (tc-expr/check part env (stx-car stx) expected-ty))
        (else
         (let-values (((penv nenv) (tc-stmt part env (stx-car stx))))
           (typing-scheme/menv nenv (tc-body/check part (env-put/env env penv) (stx-cdr stx) expected-ty))))))

;; tc-stmt : MPart Env StmtStx -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt part env stx)
  (syntax-case stx (@ @interaction @verifiably @publicly : quote def λ deftype defdata publish! verify!)
    ((@interaction _ _) (tc-stmt-interaction part env stx))
    ((@verifiably _) (tc-stmt-at-verifiably part env stx))
    ((@publicly _) (tc-stmt-at-publicly part env stx))
    ((@ p _) (identifier? #'p) (tc-stmt-at-participant part env stx))
    ((deftype . _) (tc-stmt-deftype part env stx))
    ((defdata . _) (tc-stmt-defdata part env stx))
    ((def . _) (tc-stmt-def part env stx))
    ((publish! x ...) (stx-andmap identifier? #'(x ...)) (tc-stmt-publish part env stx))
    ((verify! x ...) (stx-andmap identifier? #'(x ...)) (tc-stmt-verify part env stx))
    (expr
     (with (((typing-scheme menv _) (tc-expr part env #'expr)))
       (values empty-symdict menv)))))

;; tc-stmt-interaction : MPart Env StmtStx -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt-interaction part env stx)
  (when part
    (error 'interaction "not allowed within a specific participant"))
  (syntax-case stx (@interaction @list def λ)
    ;; TODO: when we design a separate way of calling interactions, create a separate
    ;;       type for interaction functions that forces them to be called specially
    ;;       as interactions, not just functions with extra participant arguments
    ((@interaction ((@list p ...)) (def f (λ params . body))) (stx-andmap identifier? #'(p ...))
     (tc-stmt-def part env #'(def f (λ ((p : Participant) ... . params) . body))))))

;; tc-stmt-at-verifiably : MPart Env StmtStx -> (values Env MonoEnv)
;; NOTE: This may become irrelevant depending on what desugaring
;;       happens before typechecking
(def (tc-stmt-at-verifiably part env stx)
  (syntax-case stx (@verifiably)
    ((@verifiably s)
     (let ()
       (defvalues (penv2 nenv2) (tc-stmt part env #'s))
       ;; TODO: Attach something to the new bindings in `penv2`
       ;;       so that the `verify!` form knows they can
       ;;       be verified.
       ;;       This might also involve dependencies, recording
       ;;       references to non-published identifiers that
       ;;       must be published before `verify!` can be called.
       (values penv2 nenv2)))))

;; tc-stmt-at-publicly : MPart Env StmtStx -> (values Env MonoEnv)
;; NOTE: This may become irrelevant depending on what desugaring
;;       happens before typechecking
(def (tc-stmt-at-publicly part env stx)
  (syntax-case stx (@publicly)
    ((@publicly s)
     (let ()
       (defvalues (penv2 nenv2) (tc-stmt part env #'s))
       (values (env-publish penv2 (symdict-keys penv2)) nenv2)))))

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
        (entry:type part [] (parse-type part env empty-symdict #'t))))
      empty-symdict))
    ((deftype (f 'x ...) b) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map syntax-e #'(x ...))))
       (def tyvars (list->symdict (map cons xs (map make-type:var xs))))
       (values
        (symdict
         (s
          (entry:type part xs (parse-type part env tyvars #'b))))
        empty-symdict)))))

;; tc-stmt-defdata : MPart Env StmtStx -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt-defdata part env stx)
  (syntax-case stx (@ : quote deftype defdata)
    ((defdata x variant ...) (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (def b (type:name (gensym s) []))
       (def env2 (symdict (s (entry:type part [] b))))
       (defvalues (penv nenv)
         (tc-defdata-variants part env env2 [] b #'(variant ...)))
       (values (env-put/env env2 penv) nenv)))
    ((defdata (f 'x ...) variant ...) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map syntax-e #'(x ...))))
       ;; TODO: allow variances to be either annotated or inferred
       (def vances (map (lambda (x) invariant) xs))
       (def b (type:app (type:name (gensym s) vances) (map make-type:var xs)))
       (def env2 (symdict (s (entry:type part xs b))))
       (defvalues (penv nenv)
         (tc-defdata-variants part env env2 xs b #'(variant ...)))
       (values (env-put/env env2 penv) nenv)))))


;; tc-defdata-variant : MPart Env [Listof Symbol] Type VariantStx -> Env
;; the env result contains only the new symbols introduced by the variant
(def (tc-defdata-variant part env xs b stx)
  ;; tyvars : TyvarEnv
  (def tyvars (list->symdict (map cons xs (map make-type:var xs))))
  ;; party : TypeStx -> Type
  (def (party s) (parse-type part env tyvars s))
  (syntax-case stx ()
    (x (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (symdict (s (entry:ctor part (typing-scheme empty-symdict b))))))
    ((f a ...) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (t (type:arrow (stx-map party #'(a ...)) b)))
       (symdict (s (entry:ctor part (typing-scheme empty-symdict t))))))))

;; tc-defdata-variants : MPart Env Env [Listof Symbol] Type [StxListof VariantStx] -> (values Env MonoEnv)
;; the env result contains only the new symbols introduced by the variants
(def (tc-defdata-variants part env1 env2 xs b stx)
  (def env12 (env-put/env env1 env2))
  ;; tcvariant : VariantStx -> Env
  (def (tcvariant v) (tc-defdata-variant part env12 xs b v))
  (values
   (env-put/envs empty-symdict (stx-map tcvariant stx))
   empty-symdict))

;; tc-stmt-def : MPart Env StmtStx -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt-def part env stx)
  (syntax-case stx (@ : quote def λ)
    ((def f (λ params : out-type body ...)) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map parse-param-name #'params))
           (in-ts (stx-map (lambda (p) (parse-param-type part env p)) #'params))
           (out-t (parse-type part env empty-symdict #'out-type)))
       (tc-stmt-def/typing-scheme part env s (tc-function part env xs in-ts out-t #'(body ...)))))
    ((def f (λ params body ...)) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map parse-param-name #'params))
           (in-ts (stx-map (lambda (p) (parse-param-type part env p)) #'params)))
       (tc-stmt-def/typing-scheme part env s (tc-function part env xs in-ts #f #'(body ...)))))
    ((def x : type expr) (identifier? #'x)
     (let ((s (syntax-e #'x))
           (t (parse-type part env empty-symdict #'type)))
       (tc-stmt-def/typing-scheme part env s (tc-expr/check part env #'expr t))))
    ((def x expr) (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (tc-stmt-def/typing-scheme part env s (tc-expr part env #'expr))))))

;; tc-stmt-def/typing-scheme : MPart Env Symbol TypingScheme -> (values Env MonoEnv)
;; the env result contains only the new entries introduced by the statement
(def (tc-stmt-def/typing-scheme part env s ts)
  (values
   (symdict (s (entry:known part ts)))
   (typing-scheme-menv ts)))

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
  (def body-ts (tc-body/check part body-env body exp-out-ty))
  (def body-menv (typing-scheme-menv body-ts))
  (def body-ty (typing-scheme-type body-ts))
  ;; in-tys* : [Listof NType]
  (def in-tys*
    (map (lambda (x in-ty)
           (or in-ty (symdict-ref body-menv x)))
         xs
         in-tys))
  (def menv (for/fold (acc body-menv) ((x xs) (in-ty in-tys))
              (cond (in-ty acc)
                    (else (symdict-remove acc x)))))
  (typing-scheme menv (type:arrow in-tys* body-ty)))

;; tc-stmt-verify : MPart Env StmtStx -> (values Env MonoEnv)
;; NOTE: the actual checking for verify! will be handled in the desugaring pass
;;       once that's implemented, this function should be deleted and typechecking
;;       shouldn't see verify! statements at all
(def (tc-stmt-verify part env stx)
  (syntax-case stx (verify!)
    ((verify! x ...) (stx-andmap identifier? #'(x ...))
     (let ((ts (stx-map (cut tc-expr-id part env <>) #'(x ...))))
       (values empty-symdict (menvs-meet (map typing-scheme-menv ts)))))))

;; tc-stmt-publish : MPart Env StmtStx -> (values Env MonoEnv)
(def (tc-stmt-publish part env stx)
  (syntax-case stx (publish!)
    ((publish! x ...) (stx-andmap identifier? #'(x ...))
     (let ((xs (stx-map stx-e #'(x ...))))
       (values (env-publish env xs) empty-symdict)))))

;; tc-expr : MPart Env ExprStx -> TypingScheme
(def (tc-expr part env stx)
  ;; tce : ExprStx -> TypingScheme
  (def (tce e) (tc-expr part env e))
  ;; tce/bool : ExprStx -> TypingScheme
  (def (tce/bool e) (tc-expr/check part env e type:bool))
  (syntax-case stx (@ : ann @tuple @record @list and or if block switch input digest require! assert! deposit! withdraw!)
    ((@ _ _) (error 'tc-expr "TODO: deal with @"))
    ((ann expr type)
     (tc-expr/check part env #'expr (parse-type part env empty-symdict #'type)))
    (x (identifier? #'x) (tc-expr-id part env stx))
    (lit (stx-atomic-literal? #'lit) (typing-scheme empty-symdict (tc-literal #'lit)))
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
    ((and e ...)
     (let ((ts (stx-map tce/bool #'(e ...))))
       (typing-scheme
        (menvs-meet (map typing-scheme-menv ts))
        type:bool)))
    ((or e ...)
     (let ((ts (stx-map tce/bool #'(e ...))))
       (typing-scheme
        (menvs-meet (map typing-scheme-menv ts))
        type:bool)))
    ((if c t e)
     (let ((ct (tce/bool #'c))
           (tt (tce #'t))
           (et (tce #'e)))
       (typing-scheme (menvs-meet (map typing-scheme-menv [ct tt et]))
                      (type-join (typing-scheme-type tt) (typing-scheme-type et)))))
    ((switch e swcase ...)
     (let ((ts (tc-expr part env #'e)))
       (def vt (typing-scheme-type ts))
       ;; TODO: implement exhaustiveness checking
       (def cts (stx-map (lambda (c) (tc-switch-case part env vt c)) #'(swcase ...)))
       (def bt (typing-schemes-join cts))
       (typing-scheme (menv-meet (typing-scheme-menv ts) (typing-scheme-menv bt))
                      (typing-scheme-type bt))))
    ((input type tag)
     (let ((t (parse-type part env empty-symdict #'type))
           (ts (tc-expr/check part env #'tag type:bytes)))
       (typing-scheme (typing-scheme-menv ts) t)))
    ((digest e ...)
     (let ((ts (stx-map tce #'(e ...))))
       ;; TODO: constrain the types in `ts` to types that digests
       ;;       can be computed for
       (typing-scheme (menvs-meet (map typing-scheme-menv ts))
                      type:Digest)))
    ((require! e)
     (let ((et (tce/bool #'e)))
       (typing-scheme (typing-scheme-menv et) type:unit)))
    ((assert! e)
     (let ((et (tce/bool #'e)))
       (typing-scheme (typing-scheme-menv et) type:unit)))
    ((deposit! e)
     (let ((et (tc-expr/check part env #'e type:int)))
       (typing-scheme (typing-scheme-menv et) type:unit)))
    ((withdraw! x e) (identifier? #'x)
     (let ()
       (when part (error 'withdraw! "only allowed in the consensus"))
       (def xt (tc-expr/check part env #'x type:Participant))
       (def et (tc-expr/check part env #'e type:int))
       (typing-scheme (menvs-meet (map typing-scheme-menv [xt et]))
                      type:unit)))
    ((f a ...) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (ft (tc-expr-id part env #'f)))
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
                              (cond ((identifier? a) (gensym (syntax-e a)))
                                    (else (gensym s))))
                            #'(a ...)))
          (def bv (gensym s))
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
                          (type:var bv)))))))))


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
     (let ((a (gensym s)))
       (typing-scheme (list->symdict [(cons s (type:var a))])
                      (type:var a))))
    ((entry:known _ t) t)
    ((entry:ctor _ t) t)))

;; tc-expr/check : MPart Env ExprStx (U #f NType) -> TypingSchemeOrError
;; returns expected-ty on success, actual-ty if no expected, otherwise error
(def (tc-expr/check part env stx expected-ty)
  (unless (or (not expected-ty) (ntype? expected-ty))
    (error 'tc-expr/check "expected (U #f Type) for 4th argument" expected-ty))
  (def actual-ts (tc-expr part env stx))
  (cond
    ((not expected-ty) actual-ts)
    (else
     (with (((typing-scheme menv actual-ty) actual-ts))
       (def bity (biunify [(constraint:subtype actual-ty expected-ty)] empty-symdict))
       (def ts-before (typing-scheme menv expected-ty))
       (def ts-after (typing-scheme-bisubst bity ts-before))
       ts-after))))

;; tc-literal : LiteralStx -> Type
(def (tc-literal stx)
  (def e (stx-e stx))
  (cond ((exact-integer? e) type:int)
        ((boolean? e) type:bool)
        ((string? e) type:bytes) ; represent as bytess using UTF-8
        ((bytes? e) type:bytes)
        (else (error 'tc-literal "unrecognized literal"))))

;; tc-switch-case : MPart Env Type SwitchCaseStx -> TypingScheme
(def (tc-switch-case part env valty stx)
  (syntax-case stx ()
    ((pat body ...)
     (let ((pattys (tc-pat part env valty #'pat)))
       (def env/pattys
         (symdict-put/list
          env
          (map (lambda (p) (cons (car p) (entry:known part (typing-scheme empty-symdict (cdr p)))))
               pattys)))
       (tc-body part env/pattys #'(body ...))))))


;; tc-pat : MPart Env PType PatStx -> Pattys
(def (tc-pat part env valty stx)
  (syntax-case stx (@ : ann @tuple @record @list @or-pat)
    ((@ _ _) (error 'tc-pat "TODO: deal with @"))
    ((ann pat type)
     (let ((t (parse-type part env empty-symdict #'type)))
       ;; valty <: t
       ;; because passing valty into a context expecting t
       ;; (switch (ann 5 nat) ((ann x int) x))
       (unless (subtype? valty t)
         (error 'tc-pat "type mismatch"))
       (tc-pat part env t #'pat)))
    (blank (and (identifier? #'blank) (free-identifier=? #'blank #'_))
     [])
    (x (and (identifier? #'x) (not-bound-as-ctor? env (syntax-e #'x)))
     (let ((s (syntax-e #'x)))
       [(cons s valty)]))
    (x (and (identifier? #'x) (bound-as-ctor? env (syntax-e #'x)))
     (let ((s (syntax-e #'x)))
       (def ent (symdict-ref env s))
       (unless (mpart-can-use? part (entry-part ent))
         (error s "access allowed only for" (entry-part ent)))
       (match ent
         ((entry:ctor _ (typing-scheme menv t))
          (unless (symdict-empty? menv) (error 'tc-pat "handle constructors constraining inputs"))
          ;; t doesn't necessarily have to be a supertype,
          ;; but does need to be join-compatible so that
          ;; valty <: (type-join valty t)
          (type-join valty t)
          []))))
    (lit (stx-atomic-literal? #'lit)
     (let ((t (tc-literal #'lit)))
       ;; t doesn't necessarily have to be a supertype,
       ;; but does need to be join-compatible so that
       ;; valty <: (type-join valty t)
       (type-join valty t)
       []))
    ((@or-pat p ...)
     (pattys-join (stx-map (lambda (p) (tc-pat part env valty p)) #'(p ...))))
    ((@list p ...)
     (cond
       ((type:listof? valty)
        (let ((elem-ty (type:listof-elem valty)))
          (def ptys (stx-map (lambda (p) (tc-pat part env elem-ty p)) #'(p ...)))
          (pattys-append ptys)))
       (else
        (error 'tc-pat "TODO: handle list patterns better, unification?"))))
    ((@tuple p ...)
     (match valty
       ((type:tuple ts)
        (unless (= (stx-length #'(p ...)) (length ts))
          (error 'tuple "wrong number of elements in tuple pattern"))
        (def ptys (stx-map (lambda (t p) (tc-pat part env t p)) ts #'(p ...)))
        (pattys-append ptys))
       (_ (error 'tc-pat "TODO: handle tuple patterns better, unification?"))))
    ((@record (x p) ...) (stx-andmap identifier? #'(x ...))
     (let ((s (stx-map syntax-e #'(x ...))))
       (match valty
         ((type:record fldtys)
          (def flds (symdict-keys fldtys))
          ;; s ⊆ flds
          (for (x s)
            (unless (symdict-has-key? fldtys x)
              (error x "unexpected key in record pattern")))
          ;; flds ⊆ s
          (for (f flds)
            (unless (member f s)
              (error f "missing key in record pattern")))
          (def ptys
            (stx-map (lambda (x p)
                       (tc-pat part env (symdict-ref fldtys x) p))
                     s
                     #'(p ...)))
          (pattys-append ptys))
         (_ (error 'tc-pat "TODO: handle tuple patterns better, unification?")))))
    ((f a ...) (and (identifier? #'f) (bound-as-ctor? env (syntax-e #'f)))
     (let ((s (syntax-e #'f)))
       (unless (symdict-has-key? env s)
         (error s "unbound pattern constructor"))
       (def ent (symdict-ref env s))
       (unless (mpart-can-use? part (entry-part ent))
         (error s "access allowed only for" (entry-part ent)))
       (match ent
         ((entry:ctor _ (typing-scheme menv (type:arrow in-tys out-ty)))
          (unless (symdict-empty? menv) (error 'tc-pat "handle constructors constraining inputs"))
          ;; valty <: out-ty
          ;; because passing valty into a context expecting out-ty
          (unless (subtype? valty out-ty)
            (error 'tc-pat "type mismatch"))
          (unless (= (stx-length #'(a ...)) (length in-tys))
            (error s "wrong number of arguments to data constructor"))
          (def ptys
            (stx-map (lambda (t p) (tc-pat part env t p))
                     in-tys
                     #'(a ...)))
          (pattys-append ptys)))))))

#|

(def (on-body stx)
  (syntax-case stx ()
    [() (on-expr #'(@tuple))]
    [(expr) (on-expr #'expr)]
    [(stmt body ...) (??? (on-stmt #'stmt) (on-body #'(body ...)))]))

(def (on-stmt stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! assert! deposit! withdraw!)
    [(@ attr stmt) (??? (on-stmt #'stmt))]
    [(deftype head type) (??? (on-type #'type))]
    [(defdata head variant ...) (??? (stx-map on-variant #'(variant ...)))]
    [(def id (λ params : out-type body ...)) (identifier? #'id) (??? (on-type #'out-type) (on-body #'(body ...)))]
    [(def id : type expr) (identifier? #'id) (??? (on-type #'type) (on-expr #'expr))]
    [(publish! id ...) (stx-andmap identifier? #'(id ...)) ???]
    [(verify! id ...) (stx-andmap identifier? #'(id ...)) ???]
    [_ (on-expr stx)]))

(def (on-expr stx)
  (syntax-case stx (@ : ann \@tuple \@record \@list \@or-pat if block switch _)
    [(@ attr expr) (??? (on-expr #'expr))]
    [(ann expr type) (??? (on-expr #'expr) (on-type #'type))]
    [id (identifier? #'id) ???]
    [lit (stx-atomic-literal? #'lit) ???]
    [(@list expr ...) (??? (stx-map on-expr #'(expr ...)))]
    [(@tuple expr ...) (??? (stx-map on-expr #'(expr ...)))]
    [(@record (id expr) ...) (??? (stx-map on-expr #'(expr ...)))]
    [(block body ...) (on-body #'(body ...))]
    [(if c-expr t-expr e-expr) (??? (on-expr #'c-expr) (on-expr #'t-expr) (on-expr #'e-expr))]
    [(switch expr case ...) (??? (on-expr #'expr) (on-cases #'(case ...)))]
    [(require! expr) (??? (on-expr #'expr))]
    [(assert! expr) (??? (on-expr #'expr))]
    [(deposit! expr) (??? (on-expr #'expr))]
    [(withdraw! id expr) (identifier? #'id) (??? (on-expr #'expr))]
    [(id expr ...) (identifier? #'id) (??? (stx-map on-expr #'(expr ...)))]))

(def (on-cases stx)
  (??? (stx-map on-case stx)))

(def (on-case stx)
  (syntax-case stx ()
    [(pat body ...) (??? (on-pat #'pat) (on-body #'(body ...)))]))

(def (on-pat stx)
  (syntax-case stx (@ : ann \@tuple \@record \@list \@or-pat if block switch _)
    [(@ attr pat) (??? (on-pat #'pat))]
    [(ann pat type) (??? (on-pat #'pat) (on-type #'type))]
    [id (identifier? #'id) ???]
    [_ ???]
    [lit (stx-atomic-literal? #'lit) ???]
    [(@list pat ...) (??? (stx-map on-pat #'(pat ...)))]
    [(@tuple pat ...) (??? (stx-map on-pat #'(pat ...)))]
    [(@record (id pat) ...) (??? (stx-map on-pat #'(pat ...)))]
    [(@or-pat pat ...) (??? (stx-map on-pat #'(pat ...)))]
    [(id pat ...) (identifier? #'id) (??? (stx-map on-pat #'(pat ...)))]))


(def (on-type stx)
  (syntax-case stx (@ quote \@tuple \@record)
    [(@ attr type) (??? (on-type #'type))]
    [id (identifier? #'id) ???]
    [(quote tyvar) (identifier? #'tyvar) ???]
    [(@tuple type ...) (??? (stx-map on-type #'(type ...)))]
    [(@record (id type) ...) (??? (stx-map on-type #'(type ...)))]
    [(id type ...) (identifier? #'id) (??? (stx-map on-type #'(type ...)))]))
|#

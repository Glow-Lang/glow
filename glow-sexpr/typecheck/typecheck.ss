(export #t
        (import:
         "variance.ss"
         "type.ss"))

(import :std/format
        :std/iter
        :std/misc/list
        :std/misc/repr
        :gerbil/gambit/exact
        :gerbil/gambit/bytes
        <expander-runtime>
        (for-template :gerbil/core)
        :clan/pure/dict/assq
        :clan/pure/dict/symdict
        "variance.ss"
        "type.ss")

;; Typechecking glow-sexpr

;; Type system and scoping rules are closest
;; to ReasonML, but Record types are structural.

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

;; An EnvEntry is one of:
;;  - (entry:unknown)             ; lambda-bound with no type annotation
;;  - (entry:known TypingScheme)  ; let-bound, or lambda-bound with annotation
;;  - (entry:ctor TypingScheme)   ; note ctor does not have N or P, just Type
;;  - (entry:type [Listof Symbol] Type)
(defstruct entry:unknown () transparent: #t)
(defstruct entry:known (ts) transparent: #t)
(defstruct entry:ctor (ts) transparent: #t)
(defstruct entry:type (params type) transparent: #t)
(def (env-entry? v) (or (entry:unknown? v) (entry:known? v) (entry:ctor? v) (entry:type? v)))

;; An Env is a [Symdictof EnvEntry]
;; Instead of type environments Γ, the reformulated rules use typing
;; environments Π to assign typing schemes (not type schemes) to
;; known-type/let-bound variables.

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
     (unless (eq? x y) (error 'subtype "type mismatch"))
     (unless (equal? vxs vys) (error 'subtype "inconsistant variances"))
     [])
    ((constraint:subtype (type:tuple as) (type:tuple bs))
     (map make-constraint:subtype as bs))
    ((constraint:subtype (type:record as) (type:record bs))
     (map (lambda (k)
            (constraint:subtype (symdict-ref as k) (symdict-ref bs k)))
          (symdict-keys as)))
    ((constraint:subtype (type:app (type:name f1 v1s) a1s) (type:app (type:name f2 v2s) a2s))
     (unless (eq? f1 f2) (error 'subtype "type mismatch"))
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

(def (stx-atomic-literal? v)
  (def e (stx-e v))
  (or (integer? e) (string? e) (bytes? e) (boolean? e)))

;; parse-type : Env TyvarEnv TypeStx -> Type
(def (parse-type env tyvars stx)
  ;; party : TypeStx -> Type
  (def (party s) (parse-type env tyvars s))
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
       (match (symdict-ref env s)
         ((entry:type [] t) t))))
    ((f a ...) (identifier? #'f)
     (let ((s (syntax-e #'f)))
       (match (symdict-ref env s)
         ((entry:type xs b)
          (def as (stx-map party #'(a ...)))
          (unless (= (length xs) (length as))
            (error 'parse-type "wrong number of type arguments"))
          (def tyvars2 (symdict-put/list tyvars (map cons xs as)))
          (type-subst tyvars2 b)))))))

;; parse-param-name : ParamStx -> Symbol
(def (parse-param-name p)
  (syntax-e (if (identifier? p) p (stx-car p))))

;; parse-param-type : Env ParamStx -> (U #f Type)
(def (parse-param-type env p)
  (syntax-case p (:)
    (x (identifier? #'x) #f)
    ((x : type) (parse-type env empty-symdict #'type))))

;; The reformulated rules produce judgements of the form
;; Π ⊩ e : [∆]τ
;; Function signatures follow: Env ExprStx -> TypingScheme

;; tc-body : Env BodyStx -> TypingScheme
(def (tc-body env stx)
  (cond ((stx-null? stx) (typing-scheme empty-symdict (type:tuple [])))
        ((stx-null? (stx-cdr stx)) (tc-expr env (stx-car stx)))
        (else
         (tc-body (tc-stmt env (stx-car stx)) (stx-cdr stx)))))

;; tc-body/check : Env BodyStx (U #f Type) -> TypingScheme
(def (tc-body/check env stx expected-ty)
  (cond ((stx-null? stx)
         (unless (or (not expected-ty) (subtype? (type:tuple []) expected-ty))
           (error 'tc-body/check "type mismatch with implicit unit at end of body"))
         (typing-scheme empty-symdict (or expected-ty (type:tuple []))))
        ((stx-null? (stx-cdr stx)) (tc-expr/check env (stx-car stx) expected-ty))
        (else
         (tc-body/check (tc-stmt env (stx-car stx)) (stx-cdr stx) expected-ty))))

;; tc-stmt : Env StmtStx -> Env
;; TODO: reorganize into one case for each keyword, possibly delegating to a function with
;; the multiple cases within a single keyword
(def (tc-stmt env stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
    ((@ _ _) (error 'tc-stmt "TODO: deal with @"))
    ((deftype . _) (tc-stmt-deftype env stx))
    ((defdata . _) (tc-stmt-defdata env stx))
    ((def . _) (tc-stmt-def env stx))
    ((publish! x ...) (stx-andmap identifier? #'(x ...))
     (error 'tc-stmt "TODO: deal with publish!"))
    ((verify! x ...) (stx-andmap identifier? #'(x ...))
     (error 'tc-stmt "TODO: deal with verify!"))
    (expr
     (let ()
       (tc-expr env #'expr)
       env))))

;; tc-stmt-deftype : Env StmtStx -> Env
(def (tc-stmt-deftype env stx)
  (syntax-case stx (@ : quote deftype defdata)
    ((deftype x t) (identifier? #'x)
     (symdict-put env
                  (syntax-e #'x)
                  (entry:type [] (parse-type env empty-symdict #'t))))
    ((deftype (f 'x ...) b) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map syntax-e #'(x ...))))
       (def tyvars (list->symdict (map cons xs (map make-type:var xs))))
       (symdict-put env
                    s
                    (entry:type xs (parse-type env tyvars #'b)))))))

;; tc-stmt-defdata : Env StmtStx -> Env
(def (tc-stmt-defdata env stx)
  (syntax-case stx (@ : quote deftype defdata)
    ((defdata x variant ...) (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (def b (type:name (gensym s) []))
       (tc-defdata-variants (symdict-put env s (entry:type [] b))
                            []
                            b
                            #'(variant ...))))
    ((defdata (f 'x ...) variant ...) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map syntax-e #'(x ...))))
       ;; TODO: allow variances to be either annotated or inferred
       (def vances (map (lambda (x) invariant) xs))
       (def b (type:app (type:name (gensym s) vances) (map make-type:var xs)))
       (tc-defdata-variants (symdict-put env s (entry:type xs b))
                            xs
                            b
                            #'(variant ...))))))


;; tc-defdata-variant : Env [Listof Symbol] Type VariantStx -> [Cons Symbol EnvEntry]
(def (tc-defdata-variant env xs b stx)
  ;; tyvars : TyvarEnv
  (def tyvars (list->symdict (map cons xs (map make-type:var xs))))
  ;; party : TypeStx -> Type
  (def (party s) (parse-type env tyvars s))
  (syntax-case stx ()
    (x (identifier? #'x)
     (cons (syntax-e #'x)
           (entry:ctor (typing-scheme empty-symdict b))))
    ((f a ...) (identifier? #'f)
     (cons (syntax-e #'f)
           (entry:ctor (typing-scheme empty-symdict
                                      (type:arrow (stx-map party #'(a ...)) b)))))))

;; tc-defdata-variants : Env [Listof Symbol] Type [StxListof VariantStx] -> Env
(def (tc-defdata-variants env xs b stx)
  ;; tcvariant : VariantStx -> [Cons Symbol EnvEntry]
  (def (tcvariant v) (tc-defdata-variant env xs b v))
  (symdict-put/list env (stx-map tcvariant stx)))

;; tc-stmt-def : Env StmtStx -> Env
(def (tc-stmt-def env stx)
  (syntax-case stx (@ : quote def λ)
    ((def f (λ params : out-type body ...)) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map parse-param-name #'params))
           (in-ts (stx-map (lambda (p) (parse-param-type env p)) #'params))
           (out-t (parse-type env empty-symdict #'out-type)))
       (symdict-put env s (tc-function env xs in-ts out-t #'(body ...)))))
    ((def f (λ params body ...)) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map parse-param-name #'params))
           (in-ts (stx-map (lambda (p) (parse-param-type env p)) #'params)))
       (symdict-put env s (tc-function env xs in-ts #f #'(body ...)))))
    ((def x : type expr) (identifier? #'x)
     (let ((s (syntax-e #'x))
           (t (parse-type env empty-symdict #'type)))
       (symdict-put env s (entry:known (tc-expr/check env #'expr t)))))
    ((def x expr) (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (symdict-put env s (entry:known (tc-expr env #'expr)))))))

;; tc-function : Env [Listof Symbol] [Listof (U #f Type)] (U #f Type) BodyStx -> EnvEntry
(def (tc-function env xs in-tys exp-out-ty body)
  ;; in-ents* : [Listof EnvEntry]
  ;; entry:unknown for parameters that weren't annotated
  (def in-ents*
    (map (lambda (in-ty)
           (cond (in-ty (entry:known (typing-scheme empty-symdict in-ty)))
                 (else  (entry:unknown))))
         in-tys))
  ;; body-env : Env
  (def body-env (symdict-put/list env (map cons xs in-ents*)))
  (def body-ts (tc-body/check body-env body exp-out-ty))
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
  (entry:known (typing-scheme menv (type:arrow in-tys* body-ty))))

;; tc-expr : Env ExprStx -> TypingScheme
(def (tc-expr env stx)
  ;; tce : ExprStx -> TypingScheme
  (def (tce e) (tc-expr env e))
  (syntax-case stx (@ : ann @tuple @record @list if block switch require! assert! deposit! withdraw!)
    ((@ _ _) (error 'tc-expr "TODO: deal with @"))
    ((ann expr type)
     (tc-expr/check env #'expr (parse-type env empty-symdict #'type)))
    (x (identifier? #'x) (tc-expr-id env stx))
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
     (tc-body #'(b ...)))
    ((if c t e)
     (let ((ct (tc-expr/check env #'c type:bool))
           (tt (tc-expr env #'t))
           (et (tc-expr env #'e)))
       (typing-scheme (menvs-meet (map typing-scheme-menv [ct tt et]))
                      (type-join (typing-scheme-type tt) (typing-scheme-type et)))))
    ((switch e swcase ...)
     (let ((ts (tc-expr env #'e)))
       (def vt (typing-scheme-type ts))
       ;; TODO: implement exhaustiveness checking
       (def cts (stx-map (lambda (c) (tc-switch-case env vt c)) #'(swcase ...)))
       (def bt (typing-schemes-join cts))
       (typing-scheme (menv-meet (typing-scheme-menv ts) (typing-scheme-menv bt))
                      (typing-scheme-type bt))))
    ((require! e)
     (error 'tc-expr "TODO: deal with require!"))
    ((assert! e)
     (error 'tc-expr "TODO: deal with assert!"))
    ((deposit! e)
     (error 'tc-expr "TODO: deal with deposit!"))
    ((withdraw! x e) (identifier? #'x)
     (error 'tc-expr "TODO: deal with withdraw!"))
    ((f a ...) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (ft (tc-expr-id env #'f)))
       (match (typing-scheme-type ft)
         ((type:arrow in-tys out-ty)
          (unless (= (stx-length #'(a ...)) (length in-tys))
            (error s "wrong number of arguments"))
          (def ats
            (stx-map (lambda (a t) (tc-expr/check env a t))
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
          (def atss (stx-map (lambda (a) (tc-expr env a)) #'(a ...)))
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


;; tc-expr-id : Env Identifier -> TypingScheme
(def (tc-expr-id env x)
  (def s (syntax-e x))
  (unless (symdict-has-key? env s)
    (error s "unbound identifier"))
  (match (symdict-ref env s)
    ((entry:unknown)
     (let ((a (gensym s)))
       (typing-scheme (list->symdict [(cons s (type:var a))])
                      (type:var a))))
    ((entry:known t) t)
    ((entry:ctor t) t)))

;; tc-expr/check : Env ExprStx (U #f Type) -> TypingSchemeOrError
;; returns expected-ty on success, actual-ty if no expected, otherwise error
(def (tc-expr/check env stx expected-ty)
  (unless (or (not expected-ty) (type? expected-ty))
    (error 'tc-expr/check "expected (U #f Type) for 3rd argument"))
  (def actual-ts (tc-expr env stx))
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

;; tc-switch-case : Env Type SwitchCaseStx -> TypingScheme
(def (tc-switch-case env valty stx)
  (syntax-case stx ()
    ((pat body ...)
     (let ((pattys (tc-pat env valty #'pat)))
       (def env/pattys
         (symdict-put/list
          env
          (map (lambda (p) (cons (car p) (entry:known (typing-scheme empty-symdict (cdr p)))))
               pattys)))
       (tc-body env/pattys #'(body ...))))))


;; tc-pat : Env PType PatStx -> Pattys
(def (tc-pat env valty stx)
  (syntax-case stx (@ : ann @tuple @record @list @or-pat)
    ((@ _ _) (error 'tc-pat "TODO: deal with @"))
    ((ann pat type)
     (let ((t (parse-type env empty-symdict #'type)))
       ;; valty <: t
       ;; because passing valty into a context expecting t
       ;; (switch (ann 5 nat) ((ann x int) x))
       (unless (subtype? valty t)
         (error 'tc-pat "type mismatch"))
       (tc-pat env t #'pat)))
    (blank (and (identifier? #'blank) (free-identifier=? #'blank #'_))
     [])
    (x (and (identifier? #'x) (not-bound-as-ctor? env (syntax-e #'x)))
     (let ((s (syntax-e #'x)))
       [(cons s valty)]))
    (x (and (identifier? #'x) (bound-as-ctor? env (syntax-e #'x)))
     (let ((s (syntax-e #'x)))
       (match (symdict-ref env s)
         ((entry:ctor (typing-scheme menv t))
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
     (pattys-join (stx-map (lambda (p) (tc-pat env valty p)) #'(p ...))))
    ((@list p ...)
     (cond
       ((type:listof? valty)
        (let ((elem-ty (type:listof-elem valty)))
          (def ptys (stx-map (lambda (p) (tc-pat env elem-ty p)) #'(p ...)))
          (pattys-append ptys)))
       (else
        (error 'tc-pat "TODO: handle list patterns better, unification?"))))
    ((@tuple p ...)
     (match valty
       ((type:tuple ts)
        (unless (= (stx-length #'(p ...)) (length ts))
          (error 'tuple "wrong number of elements in tuple pattern"))
        (def ptys (stx-map (lambda (t p) (tc-pat env t p)) ts #'(p ...)))
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
                       (tc-pat env (symdict-ref fldtys x) p))
                     s
                     #'(p ...)))
          (pattys-append ptys))
         (_ (error 'tc-pat "TODO: handle tuple patterns better, unification?")))))
    ((f a ...) (and (identifier? #'f) (bound-as-ctor? env (syntax-e #'f)))
     (let ((s (syntax-e #'f)))
       (unless (symdict-has-key? env s)
         (error s "unbound pattern constructor"))
       (match (symdict-ref env s)
         ((entry:ctor (typing-scheme menv (type:arrow in-tys out-ty)))
          (unless (symdict-empty? menv) (error 'tc-pat "handle constructors constraining inputs"))
          ;; valty <: out-ty
          ;; because passing valty into a context expecting out-ty
          (unless (subtype? valty out-ty)
            (error 'tc-pat "type mismatch"))
          (unless (= (stx-length #'(a ...)) (length in-tys))
            (error s "wrong number of arguments to data constructor"))
          (def ptys
            (stx-map (lambda (t p) (tc-pat env t p))
                     in-tys
                     #'(a ...)))
          (pattys-append ptys))
         (_ (error 'tc-pat "TODO: handle data constructor patterns better")))))))

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

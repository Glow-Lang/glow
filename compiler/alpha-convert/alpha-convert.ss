(export #t)

(import :std/iter
        :std/format
        :clan/pure/dict/symdict
        <expander-runtime>
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/syntax-context
        :mukn/glow/compiler/common
        :mukn/glow/compiler/alpha-convert/env
        :mukn/glow/compiler/alpha-convert/at-prefix-normalize
        :mukn/glow/compiler/alpha-convert/fresh)

;; alpha-convert

;; Rename variables so that shadowed versions are given new unique names.
;; Traverse the syntax object with an environment mapping symbols to new names.
;; On binding sites, make a new name not used before

;; Keep track of names that have been used before in a mutable hash-table.

;; Preserve the original source location

;; identifier-refer : Env Identifier -> Identifier
;; wraps the looked-up symbol in the same marks and source location
(def (identifier-refer env id)
  (restx id (symbol-refer env (stx-e id))))

;; keyword-syms : [Listof Sym]
(def keyword-syms
  '(@ : quote ann @dot @dot/type @tuple @record @list @app
    @app-ctor @or-pat @var-pat
    def deftype defdata
    if and or
    block splice switch λ
    == input digest sign
    interaction verifiably publicly
    publish! verify! require! assert! deposit! withdraw!
    ;; The following are introduced in passes between parsing and typechecking:
    ;; TODO: add support for @make-interaction @app-interaction later in this file.
    @interaction @verifiably @publicly @make-interaction @app-interaction
    ))

;; TODO: inherit this list from a map of bindings in our runtime system
;; init-syms : [Listof Sym]
(def init-syms
  '(Unit Int Nat Bool Bytes Digest Assets Signature
    not <= < > >= + - * / mod sqr sqrt
    member
    randomUInt256 isValidSignature
    canReach mustReach))

;; alpha-convert : ModuleStx -> (values ModuleStx UnusedTable AlphaBackTable Env)
(def (alpha-convert module)
  (parameterize ((current-unused-table (make-unused-table))
                 (current-alpha-back-table (make-alpha-back-table)))
    (for ((k keyword-syms)) (use/check-unused* k))
    (def init-env
      (for/fold (acc empty-symdict) ((x init-syms))
        (symdict-put acc x (entry-val (symbol-fresh* x)))))
    (syntax-case module (@module)
      ((@module stmts ...)
       (let-values (((env2 stmts2) (alpha-convert-stmts init-env (syntax->list #'(stmts ...)))))
         (values (retail-stx module stmts2)
                 (current-unused-table)
                 (current-alpha-back-table)
                 env2))))))

;; alpha-convert-type : Env TypeStx -> TypeStx
(def (alpha-convert-type env stx)
  ;; acty : TypeStx -> TypeStx
  (def (acty t) (alpha-convert-type env t))
  (syntax-case stx (@ quote @tuple @record)
    ((@ _ _) (error 'parse-type "@ annotation not allowed in type"))
    ((@tuple t ...) (retail-stx stx (stx-map acty #'(t ...))))
    ((@record (fld t) ...)
     (retail-stx stx (stx-map (lambda (fld t) (list fld (acty t))) #'(fld ...) #'(t ...))))
    ('x (identifier? #'x) stx)
    (x (identifier? #'x) (identifier-refer env #'x))
    ((f a ...) (restx stx (cons (acty #'f) (stx-map acty #'(a ...)))))))

;; alpha-convert-expr : Env ExprStx -> ExprStx
;; includes lambdas within "expressions" for the purpose of alpha-conversion
(def (alpha-convert-expr env stx)
  ;; ace : ExprStx -> ExprStx
  (def (ace e) (alpha-convert-expr env e))
  (syntax-case stx (@ ann @dot @dot/type @tuple @list @record if and or block splice == sign switch λ input digest require! assert! deposit! withdraw! @app @app-ctor)
    ((@ _ _) (error 'alpha-convert-expr "TODO: deal with @"))
    ((ann expr type)
     (retail-stx stx [(ace #'expr) (alpha-convert-type env #'type)]))
    (x (identifier? #'x) (identifier-refer env #'x))
    (lit (stx-atomic-literal? #'lit) #'lit)
    ((@dot e x) (identifier? #'x) (ac-expr-dot env stx #'e #'x))
    ((@dot/type t x) (identifier? #'x) (retail-stx stx [(alpha-convert-type env #'t) #'x]))
    ((@tuple e ...)
     (alpha-convert-keyword/sub-exprs env stx))
    ((@list e ...)
     (alpha-convert-keyword/sub-exprs env stx))
    ((@record (x e) ...)
     (and (stx-andmap identifier? #'(x ...))
          (check-duplicate-identifiers #'(x ...)))
     (retail-stx stx (stx-map (lambda (x e) [x (ace e)]) #'(x ...) #'(e ...))))
    ((block b ...)
     (retail-stx stx (alpha-convert-body env (syntax->list #'(b ...)))))
    ((splice b ...)
     (retail-stx stx (alpha-convert-body env (syntax->list #'(b ...)))))
    ((if c t e)
     (retail-stx stx [(ace #'c) (ace #'t) (ace #'e)]))
    ((and e ...)
     (alpha-convert-keyword/sub-exprs env stx))
    ((or e ...)
     (alpha-convert-keyword/sub-exprs env stx))
    ((== a b)
     (alpha-convert-keyword/sub-exprs env stx))
    ((sign e)
     (alpha-convert-keyword/sub-exprs env stx))
    ((switch e swcase ...)
     (retail-stx stx (cons (ace #'e) (stx-map (lambda (c) (ac-switch-case env c)) #'(swcase ...)))))
    ((λ . _) (ac-expr-function env stx))
    ((input type tag) (retail-stx stx [(alpha-convert-type env #'type) (ace #'tag)]))
    ((digest e ...) (alpha-convert-keyword/sub-exprs env stx))
    ((require! e) (retail-stx stx [(ace #'e)]))
    ((assert! e) (retail-stx stx [(ace #'e)]))
    ((deposit! x e) (identifier? #'x)
     (retail-stx stx [(identifier-refer env #'x) (ace #'e)]))
    ((withdraw! x e) (identifier? #'x)
     (retail-stx stx [(identifier-refer env #'x) (ace #'e)]))
    ((@app-ctor f a ...) (alpha-convert-keyword/sub-exprs env stx))
    ((@app f a ...) (alpha-convert-keyword/sub-exprs env stx))
    ((f a ...)
     (alpha-convert-keyword/sub-exprs env (intro-app stx)))))

;; ac-expr-dot : Env ExprStx ExprStx Identifier -> ExprStx
(def (ac-expr-dot env stx e fld)
  (def (w/type) (restx stx ['@dot/type (alpha-convert-type env e) fld]))
  (syntax-case e (quote)
    (x (and (identifier? #'x) (bound-as-type? env (syntax-e #'x))) (w/type))
    ('x (identifier? #'x) (w/type))
    ((tf . _) (and (identifier? #'x) (bound-as-type? env (syntax-e #'tf))) (w/type))
    (_ (retail-stx stx [(alpha-convert-expr env e) fld]))))

;; intro-app : Stx -> Stx
(def (intro-app stx)
  (unless (stx-pair? stx) (error 'intro-app "expected a function call"))
  (def f (stx-car stx))
  (restx1 stx (cons* (restx1 f '@app) f (stx-cdr stx))))

;; intro-app-ctor : Stx -> Stx
(def (intro-app-ctor stx)
  (def f (if (stx-pair? stx) (stx-car stx) stx))
  (restx1 stx (cons* (restx1 f '@app-ctor) f (if (stx-pair? stx) (stx-cdr stx) []))))

;; intro-var-pat : Stx -> Stx
(def (intro-var-pat stx) (restx1 stx [(restx1 stx '@var-pat) stx]))

;; Used by special forms with a variable number of arguments that are regular expressions,
;; like @list, @tuple, and, or.
;; It could even be used by other keywords that have a constant number of arguments, but isn't.
;; alpha-convert-body : Env StmtStx] -> StmtStx
(def (alpha-convert-keyword/sub-exprs env stx)
  (retail-stx stx (stx-map (cut alpha-convert-expr env <>) (stx-cdr stx))))

;; alpha-convert-body : Env [Listof StmtStx] -> [Listof StmtStx]
(def (alpha-convert-body env body)
  (match body
    ([] [])
    ([e] [(alpha-convert-expr env e)])
    ([stmt . rst]
     (let-values (((env2 stmt2) (alpha-convert-stmt env stmt)))
       (cons stmt2 (alpha-convert-body (env-put/env env env2) rst))))))

;; ac-expr-function : Env ExprStx -> ExprStx
(def (ac-expr-function env stx)
  (syntax-case stx (:)
    ((l params : out-type body ...)
     (let-values (((env2 params2) (ac-function-params env #'params)))
       (restx stx
              (cons* #'l params2 ': (alpha-convert-type env #'out-type)
                     (alpha-convert-body (env-put/env env env2)
                                         (syntax->list #'(body ...)))))))
    ((l params body ...)
     (let-values (((env2 params2) (ac-function-params env #'params)))
       (restx stx
              (cons* #'l params2
                     (alpha-convert-body (env-put/env env env2)
                                         (syntax->list #'(body ...)))))))))

;; ac-function-params : Env ParamsStx -> (values Env ParamsStx)
;; the env result contains only the new symbols introduced by the params
(def (ac-function-params env stx)
  (syntax-case stx ()
    ((p ...)
     (let loop ((ps (syntax->list #'(p ...))) (env2 empty-symdict) (ps2 '()))
       (match ps
         ([] (values env2 (restx stx (reverse ps2))))
         ([p1 . rst]
          (let-values (((env3 p3) (ac-function-param env p1)))
            (loop rst (env-put/env env2 env3) (cons p3 ps2)))))))))

;; ac-function-param : Env ParamStx -> (values Env ParamStx)
;; the env result contains only the new symbols introduced by the param
(def (ac-function-param env stx)
  (syntax-case stx (:)
    (x (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh* #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry-val s2)))
                 #'x2))))
    ((x : type)
     (with-syntax ((x2 (identifier-fresh* #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry-val s2)))
                 (restx stx [#'x2 ': (alpha-convert-type env #'type)])))))))

;; ac-switch-case : Env SwitchCaseStx -> SwitchCaseStx
(def (ac-switch-case env stx)
  (syntax-case stx ()
    ((pat body ...)
     (let-values (((env2 pat2) (alpha-convert-pat env #'pat)))
      (restx stx
             (cons pat2
                   (alpha-convert-body (env-put/env env env2)
                                       (syntax->list #'(body ...)))))))))

;; alpha-convert-stmts : Env [Listof StmtStx] -> (values Env [Listof StmtStx])
;; the env result contains only the new symbols introduced by the statements
(def (alpha-convert-stmts env stmts)
  (let loop ((env env) (stmts stmts) (accenv empty-symdict) (accstmts []))
    (match stmts
      ([] (values accenv (reverse accstmts)))
      ([stmt . rst]
       (let-values (((env2 stmt2) (alpha-convert-stmt env stmt)))
         (loop (env-put/env env env2) rst (env-put/env accenv env2) (cons stmt2 accstmts)))))))

;; alpha-convert-stmt : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (alpha-convert-stmt env stx)
  (syntax-case stx (@ interaction verifiably publicly @interaction @verifiably @publicly splice def deftype defdata publish! verify!)
    ((@ (interaction . _) _) (ac-stmt-atinteraction env (at-prefix-normalize stx)))
    ((@ verifiably _) (ac-stmt-wrap-simple-keyword env (at-prefix-normalize stx)))
    ((@ publicly _) (ac-stmt-wrap-simple-keyword env (at-prefix-normalize stx)))
    ((@interaction _ _) (ac-stmt-atinteraction env stx))
    ((@verifiably _) (ac-stmt-wrap-simple-keyword env stx))
    ((@publicly _) (ac-stmt-wrap-simple-keyword env stx))
    ((@ p _) (identifier? #'p) (ac-stmt-at-participant env stx))
    ((splice s ...) (ac-stmt-splice env stx))
    ((deftype . _) (ac-stmt-deftype env stx))
    ((defdata . _) (ac-stmt-defdata env stx))
    ((def . _) (ac-stmt-def env stx))
    ((publish! . _) (ac-stmt-publish env stx))
    ((verify! . _) (ac-stmt-verify env stx))
    (expr
     (values empty-symdict (alpha-convert-expr env #'expr)))))

;; ac-stmt-atinteraction : Env StmtStx -> (values Env StmtStx)
(def (ac-stmt-atinteraction env stx)
  ;; env/participants : Env [StxListof Identifier] [StxListof Identifier] -> Env
  (def (env/participants env ps ps2)
    (for/fold (env env) ((p (syntax->datum ps)) (p2 (syntax->datum ps2)))
      (symdict-put env p (entry-val p2))))
  (syntax-case stx (@list)
    ((aint ((@list p ...)) s) (stx-andmap identifier? #'(p ...))
     (let ((ps2 (stx-map identifier-fresh* #'(p ...))))
       (def env2 (env/participants env #'(p ...) ps2))
       (defvalues (env3 stmt3) (alpha-convert-stmt env2 #'s))
       (values env3 (restx stx [#'aint [(cons '@list ps2)] stmt3]))))))

;; ac-stmt-wrap-simple-keyword : Env StmtStx -> (values Env StmtStx)
(def (ac-stmt-wrap-simple-keyword env stx)
  (syntax-case stx ()
    ((k s)
     (let-values (((env2 s2) (alpha-convert-stmt env #'s)))
       (values env2 (restx stx [#'k s2]))))))

;; at-stmt-at-participant : Env StmtStx -> (values Env StmtStx)
(def (ac-stmt-at-participant env stx)
  (syntax-case stx (@)
    ((a p s) (identifier? #'p)
     (with-syntax ((p2 (identifier-refer env #'p)))
       (let-values (((env2 s2) (alpha-convert-stmt env #'s)))
         (values env2 (restx stx [#'a #'p2 s2])))))))

;; ac-stmt-splice : Env StmtStx -> (values Env StmtStx)
(def (ac-stmt-splice env stx)
  (syntax-case stx ()
    ((spl s ...)
     (let-values (((env2 s2) (alpha-convert-stmts env (syntax->list #'(s ...)))))
       (values env2 (restx stx (cons #'spl s2)))))))

;; ac-stmt-deftype : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (ac-stmt-deftype env stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
    ((dt x t) (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh* #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry-type s2)))
                 (restx stx [#'dt #'x2 (alpha-convert-type env #'t)])))))
    ((dt (f . xs) b) (identifier? #'f)
     (with-syntax ((f2 (identifier-fresh* #'f)))
       (let ((s (syntax-e #'f)) (s2 (syntax-e #'f2)))
         (values (symdict (s (entry-type s2)))
                 (restx stx [#'dt (cons #'f2 #'xs) (alpha-convert-type env #'b)])))))))

;; ac-stmt-defdata : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (ac-stmt-defdata env stx)
  (defvalues (lhs variants rtvalue)
    (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
      ((_ lhs variant ... with: rtval)
       (values #'lhs #'(variant ...) #'rtval))
      ((_ lhs variant ...)
       (values #'lhs #'(variant ...) #f))))
  (def id (definition-lhs->id lhs))
  (def alpha-id (identifier-fresh* id))
  (def name (syntax-e id))
  (def alpha-name (syntax-e alpha-id))
  (def alpha-lhs
    (syntax-case lhs ()
      ;; TODO: also a-c the type parameters? In the same/another namespace?
      ((id . args) (identifier? #'id) (restx lhs (cons alpha-id #'args)))
      (id (identifier? #'id) alpha-id)))
  (def env2 (symdict (name (entry-type alpha-name))))
  (defvalues (env3 variants3) (ac-defdata-variants env env2 variants))
  (def env4 (env-put/env env2 env3))
  (if rtvalue
    (values env4 (retail-stx stx `(,alpha-lhs ,@variants3
                                   with: ,(alpha-convert-expr (env-put/env env env4) rtvalue))))
    (values env4 (retail-stx stx `(,alpha-lhs ,@variants3)))))

;; ac-defdata-variants : Env Env [StxListof VariantStx] -> (values Env [Listof VariantStx])
;; the env result contains only the new symbols introduced by the variants
(def (ac-defdata-variants env1 env2 variants)
  (def env12 (env-put/env env1 env2))
  (let loop ((variants variants) (env3 empty-symdict) (vs3 []))
    (match variants
      ([] (values env3 (reverse vs3)))
      ([v . rst]
       (defvalues (env4 v4) (ac-defdata-variant env12 v))
       (loop rst (env-put/env env3 env4) (cons v4 vs3))))))

;; ac-defdata-variant : Env VariantStx -> (values Env VariantStx)
;; the env result contains only the new symbols introduced by the variant
(def (ac-defdata-variant env stx)
  ;; acty : TypeStx -> TypeStx
  (def (acty t) (alpha-convert-type env t))
  (syntax-case stx ()
    (x (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh* #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry-ctor s2)))
                 #'x2))))
    ((f a ...) (identifier? #'f)
     (with-syntax ((f2 (identifier-fresh* #'f)))
       (let ((s (syntax-e #'f)) (s2 (syntax-e #'f2)))
         (values (symdict (s (entry-ctor s2)))
                 (restx stx (cons #'f2 (stx-map acty #'(a ...))))))))))

;; ac-stmt-def : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (ac-stmt-def env stx)
  (syntax-case stx (@ : quote def λ)
    ((d x : type expr) (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh* #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry-val s2)))
                 (restx stx [#'d #'x2 ':
                             (alpha-convert-type env #'type)
                             (alpha-convert-expr env #'expr)])))))
    ((d x expr) (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh* #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry-val s2)))
                 (restx stx [#'d #'x2 (alpha-convert-expr env #'expr)])))))))

;; ac-stmt-publish : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (ac-stmt-publish env stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
    ((pub x ...) (stx-andmap identifier? #'(x ...))
     (values
      empty-symdict
      (restx stx (cons #'pub (stx-map (lambda (x) (identifier-refer env x)) #'(x ...))))))))

;; ac-stmt-verify : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (ac-stmt-verify env stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
    ((veri x ...) (stx-andmap identifier? #'(x ...))
     (values
      empty-symdict
      (restx stx (cons #'veri (stx-map (lambda (x) (identifier-refer env x)) #'(x ...))))))))

;; alpha-convert-pat : Env PatStx -> (values Env PatStx)
;; the env result contains only the new symbols introduced by the pattern
(def (alpha-convert-pat env stx)
  (syntax-case stx (@ : ann @tuple @record @list @or-pat @var-pat @app-ctor)
    ((@ _ _) (error 'alpha-convert-pat "TODO: deal with @"))
    ((ann pat type)
     (let-values (((env2 pat2) (alpha-convert-pat env #'pat)))
       (values env2
               (retail-stx stx [pat2 (alpha-convert-type env #'type)]))))
    (blank (and (identifier? #'blank) (free-identifier=? #'blank #'_))
     (values empty-symdict stx))
    (lit (stx-atomic-literal? #'lit)
     (values empty-symdict stx))
    (x (and (identifier? #'x) (bound-as-ctor? env (syntax-e #'x)))
     (ac-pat-app-ctor env (intro-app-ctor stx)))
    (x (and (identifier? #'x) (not-bound-as-ctor? env (syntax-e #'x)))
     (ac-pat-var-pat env (intro-var-pat stx)))
    ((@var-pat . _)
     (ac-pat-var-pat env stx))
    ((@or-pat p ...)
     (let-values (((env2 ps2) (ac-pats-join env (syntax->list #'(p ...)))))
       (values env2 (retail-stx stx ps2))))
    ((@list . _)
     (ac-pat-simple-seq env stx))
    ((@tuple . _)
     (ac-pat-simple-seq env stx))
    ((@record (x p) ...) (stx-andmap identifier? #'(x ...))
     (let-values (((env2 ps2) (ac-pats-meet env (syntax->list #'(p ...)))))
       (values env2 (retail-stx stx (stx-map list #'(x ...) ps2)))))
    ((@app-ctor f a ...)
     (ac-pat-app-ctor env stx))
    ((f a ...) (and (identifier? #'f) (bound-as-ctor? env (syntax-e #'f)))
     (ac-pat-app-ctor env (intro-app-ctor stx)))))

;; ac-pat-var-pat : Env PatStx -> (values Env PatStx)
(def (ac-pat-var-pat env stx)
  (syntax-case stx ()
    ((_ x) (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh* #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry-val s2)))
                 (retail-stx stx [#'x2])))))))

;; ac-pat-app-ctor : Env PatStx -> (values Env PatStx)
(def (ac-pat-app-ctor env stx)
  (syntax-case stx ()
    ((_ f a ...) (and (identifier? #'f) (bound-as-ctor? env (syntax-e #'f)))
     (with-syntax ((f2 (identifier-refer env #'f)))
       (let-values (((env2 ps2) (ac-pats-meet env (syntax->list #'(a ...)))))
         (values env2 (retail-stx stx (cons #'f2 ps2))))))))

;; ac-pat-simple-seq : Env PatStx -> (values Env PatStx)
;; the env result contains only the new symbols introduced by the pattern
;; used for @list and @tuple
(def (ac-pat-simple-seq env stx)
  (syntax-case stx ()
    ((hd p ...)
     (let-values (((env2 ps2) (ac-pats-meet env (syntax->list #'(p ...)))))
       (values env2 (retail-stx stx ps2))))))

;; ac-pats-meet : Env [Listof PatStx] -> (values Env [Listof PatStx])
;; the env result contains only the new symbols introduced by the patterns
;; used for @list, @tuple, @record, and ctors, but not @or-pat
(def (ac-pats-meet env ps)
  (let loop ((ps ps) (env2 empty-symdict) (ps2 '()))
    (match ps
      ([] (values env2 (reverse ps2)))
      ([p1 . rst]
       (let-values (((env3 p3) (alpha-convert-pat env p1)))
         (loop rst (env-put/env env2 env3) (cons p3 ps2)))))))

;; ac-pats-join : Env [Listof PatStx] -> (values Env [Listof PatStx])
;; the env result contains only the new symbols that are *common* between all the pats
;; symbols that are shared by all are renamed the same in all
;; symbols that are not shared by all are renamed differently in each one
;; used for @or-pat
(def (ac-pats-join env pats)
  ;; strategy, two pases:
  ;;  * traverse pats to get pattern-variable-symbols
  ;;  * alpha-convert pats using an env binding the common symbols as ctors
  (def syms
    (symbol-list-set-intersect
     (map (lambda (p) (pattern-variable-symbols env p)) pats)))
  (def new-syms (map symbol-fresh* syms))
  (def env/vars
    (for/fold (acc empty-symdict) ((s1 syms) (s2 new-syms))
      (symdict-put acc s1 (entry-val s2))))
  (def env/ctors
    (for/fold (acc env) ((s1 syms) (s2 new-syms))
      (symdict-put acc s1 (entry-ctor s2))))
  (values env/vars
          (map (lambda (p)
                 (let-values (((env3 p3) (alpha-convert-pat env/ctors p)))
                   p3))
               pats)))

;; pattern-variable-symbols : Env PatStx -> [Listof Symbol]
(def (pattern-variable-symbols env pat)
  ;; copy because mutable, don't mutate existing, mutate copy instead
  (parameterize ((current-unused-table (copy-current-unused-table))
                 (current-alpha-back-table (copy-current-alpha-back-table)))
    (let-values (((env2 pat2) (alpha-convert-pat env pat)))
      (symdict-keys env2))))

;; symbol-list-set-intersect : [Listof [Listof Symbol]] -> [Listof Symbol]
(def (symbol-list-set-intersect ls)
  (match ls
    ([] [])
    ([l] l)
    ([l1 . rst]
     (filter (lambda (s)
               (andmap (lambda (l) (memq s l)) rst))
             l1))))

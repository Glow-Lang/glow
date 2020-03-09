(export #t)

(import :std/iter
        :clan/pure/dict/symdict
        <expander-runtime>
        (for-template :gerbil/core)
        "../common.ss"
        "symbolnat.ss")

;; alpha-convert

;; Rename variables so that shadowed versions are given new unique names.
;; Traverse the syntax object with an environment mapping symbols to new names.
;; On binding sites, make a new name not used before

;; Keep track of names that have been used before in a mutable hash-table.

;; Preserve the original source location

;; head-id : Stx -> (U Id #f)
(def (head-id stx)
  (cond ((identifier? stx) stx)
        ((stx-pair? stx) (head-id (stx-car stx)))
        (else #f)))

;; restx1 : Stx Any -> Stx
(def (restx1 stx e)
  (datum->syntax (head-id stx) e (stx-source stx)))

;; restx : Stx Any -> Stx
;; multiple levels for a nested list / tree
(def (restx stx e)
  (cond ((and (list? e) (stx-list? stx) (= (length e) (stx-length stx)))
         (restx1 stx (stx-map restx stx e)))
        ((and (pair? e) (stx-pair? stx))
         (let ((p (cons (restx (stx-car stx) (car e))
                        (restx (stx-cdr stx) (cdr e)))))
           (if (pair? stx) p (restx1 stx p))))
        (else
         (restx1 stx e))))

;; An UnusedTable is a [Hashof Symbol UnusedList]
;; Keys are symbols that do not end in numbers.
;; Values are lists where unused nats can be appended with
;; the key to make an unused symbol.
;; make-unused-table : -> UnusedTable
(def (make-unused-table) (make-hash-table-eq))

;; current-unused-table : [Parameterof UnusedTable]
(def current-unused-table (make-parameter (make-unused-table)))

;; copy-current-unused-table : -> UnusedTable
(define (copy-current-unused-table) (hash-copy (current-unused-table)))

;; symbol-fresh : Symbol -> Symbol
;; finds an symbol not used so far, marks it used, and returns it
(def (symbol-fresh sym)
  (unless (symbol? sym)
    (error 'symdol-fresh "expected symbol"))
  (let-values (((s n) (symbol-split sym)))
    (def ut (current-unused-table))
    (def ul (hash-ref ut s []))
    (cond ((unusedlist-unused? ul n)
           (hash-put! ut s (unusedlist-remove ul n))
           (symbolnat s n))
          (else
           (hash-put! ut s (unusedlist-rest ul))
           (symbolnat s (unusedlist-first ul))))))

;; identifier-fresh : Identifer -> Identifier
;; wraps the freshened symbol in the same marks and source location
(def (identifier-fresh id)
  (unless (identifier? id)
    (error 'identifier-fresh "expected identifier"))
  (restx id (symbol-fresh (stx-e id))))

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

;; identifier-refer : Env Identifier -> Identifier
;; wraps the looked-up symbol in the same marks and source location
(def (identifier-refer env id)
  (restx id (symbol-refer env (stx-e id))))

;; init-syms : [Listof Sym]
(def init-syms '(int bool bytes not < + sqr sqrt member))

;; alpha-convert-prog : [Listof StmtStx] -> (values Env [Listof StmtStx])
(def (alpha-convert-prog stmts)
  (parameterize ((current-unused-table (make-unused-table)))
    (def init-env
      (for/fold (acc empty-symdict) ((x init-syms))
        (symdict-put acc x (entry (symbol-fresh x) #f))))
    (let loop ((env init-env) (stmts stmts) (acc []))
      (match stmts
        ([] (values env (reverse acc)))
        ([stmt . rst]
         (let-values (((env2 stmt2) (alpha-convert-stmt env stmt)))
           (loop (env-put/env env env2) rst (cons stmt2 acc))))))))

;; alpha-convert-type : Env TypeStx -> TypeStx
(def (alpha-convert-type env stx)
  ;; acty : TypeStx -> TypeStx
  (def (acty t) (alpha-convert-type env t))
  (syntax-case stx (@ quote @tuple @record)
    ((@ _ _) (error 'parse-type "TODO: deal with @"))
    ((@tuple t ...)
     (restx stx (cons (stx-car stx) (stx-map acty #'(t ...)))))
    ((@record (fld t) ...)
     (restx stx
            (cons (stx-car stx)
                  (stx-map (lambda (fld t) (list fld (acty t)))
                           #'(fld ...)
                           #'(t ...)))))
    ('x (identifier? #'x) stx)
    (x (identifier? #'x) (identifier-refer env #'x))
    ((f a ...) (restx stx (cons (acty #'f) (stx-map acty #'(a ...)))))))

;; alpha-convert-expr : Env ExprStx -> ExprStx
;; includes lambdas within "expressions" for the purpose of alpha-conversion
(def (alpha-convert-expr env stx)
  ;; ace : ExprStx -> ExprStx
  (def (ace e) (alpha-convert-expr env e))
  (syntax-case stx (@ : ann @tuple @record @list if block switch λ require! assert! deposit! withdraw!)
    ((@ _ _) (error 'alpha-convert-expr "TODO: deal with @"))
    ((ann expr type)
     (restx stx [(stx-car stx) (ace #'expr) (alpha-convert-type env #'type)]))
    (x (identifier? #'x) (identifier-refer env #'x))
    (lit (stx-atomic-literal? #'lit) #'lit)
    ((@tuple e ...)
     (restx stx (cons (stx-car stx) (stx-map ace #'(e ...)))))
    ((@list e ...)
     (restx stx (cons (stx-car stx) (stx-map ace #'(e ...)))))
    ((@record (x e) ...)
     (and (stx-andmap identifier? #'(x ...))
          (check-duplicate-identifiers #'(x ...)))
     (restx stx
            (cons (stx-car stx)
                  (stx-map (lambda (x e) [x (ace e)]) #'(x ...) #'(e ...)))))
    ((block b ...)
     (restx stx (cons (stx-car stx) (alpha-convert-body env (syntax->list #'(b ...))))))
    ((if c t e)
     (restx stx [(stx-car stx) (ace #'c) (ace #'t) (ace #'e)]))
    ((switch e swcase ...)
     (let ((e2 (ace #'e)))
       (restx stx
              (cons* (stx-car stx) (ace #'e)
                     (stx-map (lambda (c) (ac-switch-case env c)) #'(swcase ...))))))
    ((λ . _)
     (ac-expr-function env stx))
    ((require! e)
     (restx stx [(stx-car stx) (ace #'e)]))
    ((assert! e)
     (restx stx [(stx-car stx) (ace #'e)]))
    ((deposit! e)
     (restx stx [(stx-car stx) (ace #'e)]))
    ((withdraw! x e) (identifier? #'x)
     (restx stx [(stx-car stx) (identifier-refer env #'x) (ace #'e)]))
    ((f a ...) (identifier? #'f)
     (restx stx (cons (ace #'f) (stx-map ace #'(a ...)))))))

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
  (syntax-case stx (@ : ann @tuple @record @list if block switch λ require! assert! deposit! withdraw!)
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
     (with-syntax ((x2 (identifier-fresh #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry s2 #f)))
                 #'x2))))
    ((x : type)
     (with-syntax ((x2 (identifier-fresh #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry s2 #f)))
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

;; alpha-convert-stmt : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (alpha-convert-stmt env stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
    ((@ _ _) (error 'alpha-convert-stmt "TODO: deal with @"))
    ((deftype . _) (ac-stmt-deftype env stx))
    ((defdata . _) (ac-stmt-defdata env stx))
    ((def . _) (ac-stmt-def env stx))
    ((publish! . _) (ac-stmt-publish env stx))
    ((verify! . _) (ac-stmt-verify env stx))
    (expr
     (values empty-symdict (alpha-convert-expr env #'expr)))))

;; ac-stmt-deftype : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (ac-stmt-deftype env stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
    ((dt x t) (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry s2 #f)))
                 (restx stx [#'dt #'x2 (alpha-convert-type env #'t)])))))
    ((dt (f . xs) b) (identifier? #'f)
     (with-syntax ((f2 (identifier-fresh #'f)))
       (let ((s (syntax-e #'f)) (s2 (syntax-e #'f2)))
         (values (symdict (s (entry s2 #f)))
                 (restx stx [#'dt (cons #'f2 #'xs) (alpha-convert-type env #'b)])))))))

;; ac-stmt-defdata : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (ac-stmt-defdata env stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
    ((dd x variant ...) (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (def env2 (symdict (s (entry s2 #f))))
         (defvalues (env3 variants3) (ac-defdata-variants env env2 #'(variant ...)))
         (values (env-put/env env2 env3)
                 (restx stx [#'dd #'x2 . variants3])))))
    ((dd (f . xs) variant ...) (identifier? #'f)
     (with-syntax ((f2 (identifier-fresh #'f)))
       (let ((s (syntax-e #'f)) (s2 (syntax-e #'f2)))
         (def env2 (symdict (s (entry s2 #f))))
         (defvalues (env3 variants3) (ac-defdata-variants env env2 #'(variant ...)))
         (values (env-put/env env2 env3)
                 (restx stx [#'dd (cons #'f2 #'xs) . variants3])))))))

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
     (with-syntax ((x2 (identifier-fresh #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry s2 #t)))
                 #'x2))))
    ((f a ...) (identifier? #'f)
     (with-syntax ((f2 (identifier-fresh #'f)))
       (let ((s (syntax-e #'f)) (s2 (syntax-e #'f2)))
         (values (symdict (s (entry s2 #t)))
                 (restx stx (cons #'f2 (stx-map acty #'(a ...))))))))))

;; ac-stmt-def : Env StmtStx -> (values Env StmtStx)
;; the env result contains only the new symbols introduced by the statement
(def (ac-stmt-def env stx)
  (syntax-case stx (@ : quote def λ)
    ((d x : type expr) (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry s2 #f)))
                 (restx stx [#'d #'x2 ':
                             (alpha-convert-type env #'type)
                             (alpha-convert-expr env #'expr)])))))
    ((d x expr) (identifier? #'x)
     (with-syntax ((x2 (identifier-fresh #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry s2 #f)))
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
  (syntax-case stx (@ : ann @tuple @record @list @or-pat)
    ((@ _ _) (error 'alpha-convert-pat "TODO: deal with @"))
    ((ann pat type)
     (let-values (((env2 pat2) (alpha-convert-pat env #'pat)))
       (values env2
               (restx stx [(stx-car stx) pat2 (alpha-convert-type env #'type)]))))
    (blank (and (identifier? #'blank) (free-identifier=? #'blank #'_))
     (values empty-symdict stx))
    (lit (stx-atomic-literal? #'lit)
     (values empty-symdict stx))
    (x (and (identifier? #'x) (bound-as-ctor? env (syntax-e #'x)))
     (values empty-symdict (identifier-refer env #'x)))
    (x (and (identifier? #'x) (not-bound-as-ctor? env (syntax-e #'x)))
     (with-syntax ((x2 (identifier-fresh #'x)))
       (let ((s (syntax-e #'x)) (s2 (syntax-e #'x2)))
         (values (symdict (s (entry s2 #f)))
                 #'x2))))
    ((@or-pat p ...)
     (let-values (((env2 ps2) (ac-pats-join env (syntax->list #'(p ...)))))
       (values env2
               (restx stx (cons (stx-car stx) ps2)))))
    ((@list . _)
     (ac-pat-simple-seq env stx))
    ((@tuple . _)
     (ac-pat-simple-seq env stx))
    ((@record (x p) ...) (stx-andmap identifier? #'(x ...))
     (let-values (((env2 ps2) (ac-pats-meet env (syntax->list #'(p ...)))))
       (values env2
               (restx stx (cons (stx-car stx) (stx-map list #'(x ...) ps2))))))
    ((f a ...) (and (identifier? #'f) (bound-as-ctor? env (syntax-e #'f)))
     (with-syntax ((f2 (identifier-refer env #'f)))
       (let-values (((env2 ps2) (ac-pats-meet env (syntax->list #'(a ...)))))
         (values env2
                 (restx stx (cons #'f ps2))))))))

;; ac-pat-simple-seq : Env PatStx -> (values Env PatStx)
;; the env result contains only the new symbols introduced by the pattern
;; used for @list and @tuple
(def (ac-pat-simple-seq env stx)
  (syntax-case stx ()
    ((hd p ...)
     (let-values (((env2 ps2) (ac-pats-meet env (syntax->list #'(p ...)))))
       (values env2 (restx stx (cons #'hd ps2)))))))

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
  (def new-syms (map symbol-fresh syms))
  (def env/vars
    (for/fold (acc empty-symdict) ((s1 syms) (s2 new-syms))
      (symdict-put acc s1 (entry s2 #f))))
  (def env/ctors
    (for/fold (acc env) ((s1 syms) (s2 new-syms))
      (symdict-put acc s1 (entry s2 #t))))
  (values env/vars
          (map (lambda (p)
                 (let-values (((env3 p3) (alpha-convert-pat env/ctors p)))
                   p3))
               pats)))

;; pattern-variable-symbols : Env PatStx -> [Listof Symbol]
(def (pattern-variable-symbols env pat)
  ;; copy because mutable, don't mutate existing, mutate copy instead
  (parameterize ((current-unused-table (copy-current-unused-table)))
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

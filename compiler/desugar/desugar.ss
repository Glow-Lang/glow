(export #t)

(import :std/format :std/iter
        :std/misc/list :std/srfi/1
        :std/misc/repr :clan/debug ;; DEBUG
        <expander-runtime>
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/syntax-context
        :mukn/glow/compiler/alpha-convert/fresh
        :mukn/glow/compiler/common)

;; Desugaring away these: @verifiably! verify! @publicly! defdata deftype and or
;; In the future, let users control desuraging of defdata and deftype with @deriving annotations (?)

;; desugar-stmts : [Listof StmtStx] -> [Listof StmtStx]
(def (desugar-stmts stmts) (unsplice-stmts (map desugar-stmt stmts)))

;; desugar-stmt : StmtStx -> StmtStx
;; accumulate new reduced statements for the current unreduced statement,
;; in reversed order at the beginning of the accumulator acc
(def (desugar-stmt stx)
  (syntax-case stx (@ @debug-label @interaction @verifiably! @publicly! deftype defdata publish! def λ)
    ((@debug-label . _) stx)
    ((@interaction _ (def _ (λ . _))) (desugar-def-interaction stx))
    ((@verifiably! (p) definition) (restx stx [#'@ #'p (desugar-verifiably stx #'p #'definition)]))
    ((@publicly! (p) definition) (desugar-publicly stx #'p #'definition))
    ((@ p s) (identifier? #'p)
     (syntax-case #'s (splice)
       ((splice . body) (retail-stx #'s (map-in-order (λ (x) (desugar-stmt (restx x [#'@ #'p x])))
                                                      (syntax->list #'body))))
       (_ (retail-stx stx [#'p (desugar-stmt #'s)]))))
    ((defdata . _) (desugar-defdata stx))
    ((deftype . _) (desugar-deftype stx))
    ((publish! . _) (desugar-publish! stx))
    ((def . _) (desugar-def stx))
    (expr (desugar-expr stx))))

(def (desugar-def-interaction stx)
  (defvalues (ip v lp ot body)
    (syntax-case stx (:)
      ((@interaction iparams (def v (lam lparams : out-type . body)))
       (values #'iparams #'v #'lparams #'(: out-type) #'body))
      ((@interaction iparams (def v (lam lparams . body)))
       (values #'iparams #'v #'lparams #'() #'body))))
  (restx1 stx [#'def v '() (restx1 stx [#'@make-interaction ip lp ot (desugar-body body) ...])]))

(def (nat-to-variants variants)
  (let loop ((i 0) (acc []) (variants variants))
    (def (continue x r) (loop (+ i 1) (cons [i x] acc) r))
    (syntax-case variants ()
      (() (reverse acc))
      (((x) . r) (identifier? #'x) (continue #'x #'r))
      ((x . r) (identifier? #'x) (continue #'x #'r))
      (_ #f))))

(def (desugar-defdata stx)
  (syntax-case stx ()
    ((defdata spec variant ... with: rtvalue)
     stx)
    ((defdata spec variant ...)
     (begin
       (def (mk-var x)
         (restx stx (symbol-fresh x)))
       (def input
         ;; TODO: support input for types with type-parameters
         (if (identifier? #'spec)
           (let ((x (mk-var 'x))
                 (tag (mk-var 'tag)))
             [['input (restx stx [#'λ [tag] [': #'spec] [#'def x [': #'spec] [#'input #'spec tag]] x])]])
           []))
       (def toofNat
         (let ((ofNat-cases (nat-to-variants #'(variant ...))))
           (if ofNat-cases
             (let ((of-x (mk-var 'x))
                   (x-to (mk-var 'x))
                   (toNat-cases (for/collect ((c ofNat-cases))
                                  (with (([n d] c)) [['@app-ctor d] n]))))
               ;; TODO: use some magic sum-index internal, not switches
               [['toNat [#'λ [[x-to ': #'spec]] [': 'Nat] [#'switch x-to . toNat-cases]]]
                ['ofNat [#'λ [[of-x ': #'Nat]] [': #'spec] [#'switch of-x . ofNat-cases]]]])
             '())))
       (def spec-name (stx-e (if (identifier? #'spec) #'spec (stx-car #'spec))))
       (def rtvalue `(@record ,@input ,@toofNat))
       (retail-stx stx `(,#'spec ,@(syntax->list #'(variant ...)) with: ,rtvalue))))))

(def (desugar-publish! stx)
  (syntax-case stx ()
    ((_ participant variable) stx)
    ((_ participant variable ...)
     (with-syntax (((pubvar ...) (stx-map (lambda (v) (retail-stx stx [#'participant v])) #'(variable ...))))
       (restx1 stx #'(splice pubvar ...))))))

;; TODO: input, isA, JSON converters, EthBytes converters, etc.
;; desugar-deftype : Stx -> Stx
(def (desugar-deftype stx)
  stx)

(def current-verifications (make-parameter (make-hash-table)))

(def (expression-verifiable? expr)
  ;; TODO: check that expr is a computation all made of verifiable elements,
  ;; and does not require change of participants within the computation,
  ;; and can fit in one transaction(?)
  #t)

(def (computation-verification expr)
  (unless (expression-verifiable? expr) (error 'expression-not-verifiable expr))
  expr)

(def (make-verification p var expr)
  (restx expr
    (syntax-case expr (sign)
      ((sign msg) [#'@app #'isValidSignature p (computation-verification #'msg) var])
      (_ [#'== var (computation-verification expr)]))))

;; desugar-verifiably : Identifier Stx -> Stx
(def (desugar-verifiably stx p definition)
  ;;(std/misc/repr#prn ['desugar-verifiably stx p definition])
  (syntax-case definition (def)
    ((def name expr)
     (begin
       (hash-put! (current-verifications) (syntax-e #'name)
                  [#'require! (restx stx (make-verification p #'name #'expr))])
       (desugar-def definition)))))

;; verify-var : Identifier -> Expr
(def (verify-var var)
  (let ((verification (hash-get (current-verifications) (syntax-e var))))
    (unless verification
      ;; TODO: properly report location, etc.
      (error "cannot verify variable not defined verifiably" var))
    (restx var verification)))

;; desugar-verify : Stx -> Stx
(def (desugar-verify vars)
  (cons #'splice (map verify-var (syntax->list vars))))

;; desugar-publicly : Stx -> Stx
(def (desugar-publicly stx p definition)
  (syntax-case definition (def)
    ((def name expr)
     (restx1 stx [#'splice [#'@ p (desugar-verifiably stx p definition)]
                           [#'publish! p #'name]
                           (desugar-verify [#'name])]))))

;; desugar-def : Stx -> Stx
(def (desugar-def stx)
  (syntax-case stx (:)
    ((def name : type expr)
     (retail-stx stx [#'name [': #'type] (desugar-expr #'expr)]))
    ((def name expr)
     (retail-stx stx [#'name [] (desugar-expr #'expr)]))))

;; desugar-expr : Stx -> Stx
(def (desugar-expr stx)
  (syntax-case stx (@ ann @dot @dot/type @tuple @record @list if and or block splice switch λ == input digest sign require! assert! deposit! withdraw! verify! @app)
    ((@ _ _) (error 'desugar-expr "TODO: deal with @"))
    ((ann expr type) (retail-stx stx [(desugar-expr #'expr) #'type]))
    (x (identifier? #'x) stx)
    (lit (stx-atomic-literal? #'lit) #'lit)
    ((@dot e x) (identifier? #'x) (retail-stx stx [(desugar-expr #'e) #'x]))
    ((@dot/type t x) (identifier? #'x) stx)
    ((@tuple e ...) (desugar-keyword/sub-exprs stx))
    ((@list e ...) (desugar-keyword/sub-exprs stx))
    ((@record (x e) ...) (retail-stx stx (stx-map (lambda (x e) [x (desugar-expr e)]) #'(x ...) #'(e ...))))
    ((block b ...) (retail-stx stx (desugar-body #'(b ...))))
    ((splice b ...) (retail-stx stx (desugar-body #'(b ...))))
    ((if c t e) (restx1 stx [#'switch (desugar-expr #'c)
                                      [#t (desugar-expr #'t)] [#f (desugar-expr #'e)]]))
    ((and) (restx1 stx #t))
    ((and e) #'e)
    ((and e1 e2 ...)
     (restx1 stx [#'switch (desugar-expr #'e1)
                           [#f #f] [#t (desugar-expr (restx1 stx #'(and e2 ...)))]]))
    ((or) (restx stx #f))
    ((or e) #'e)
    ((or e1 e2 ...)
     (restx1 stx [#'switch (desugar-expr #'e1)
                           [#t #t] [#f (desugar-expr (restx1 stx #'(and e2 ...)))]]))
    ((switch e swcase ...)
     (retail-stx stx (cons (desugar-expr #'e) (stx-map desugar-switch-case #'(swcase ...)))))
    ((λ . _) (desugar-lambda stx))
    ((== a b) (desugar-keyword/sub-exprs stx))
    ((input type tag) (retail-stx stx [#'type (desugar-expr #'tag)]))
    ((digest e ...) (desugar-keyword/sub-exprs stx))
    ((sign e) (desugar-keyword/sub-exprs stx))
    ((require! e) (desugar-keyword/sub-exprs stx))
    ((assert! e) (desugar-keyword/sub-exprs stx))
    ((deposit! _ x e) (desugar-keyword/sub-exprs stx))
    ((withdraw! _ x e) (desugar-keyword/sub-exprs stx))
    ((verify! x ...) (desugar-verify #'(x ...)))
    ((@app a ...) (desugar-keyword/sub-exprs stx))))

(def (desugar-keyword/sub-exprs stx)
  (retail-stx stx (stx-map desugar-expr (stx-cdr stx))))

;; Stx -> [Listof Stx]
(def (desugar-body body)
  (desugar-stmts (syntax->list body)))

(def (desugar-switch-case stx)
  (syntax-case stx () ((pat body ...) (retail-stx stx (desugar-body #'(body ...))))))

(def (desugar-lambda stx)
  (defvalues (params ?out-type body)
    (syntax-case stx (:)
      ((_ params : out-type body ...) (values #'params #'(: out-type) #'(body ...)))
      ((_ params body ...) (values #'params #'() #'(body ...)))))
  (retail-stx stx (cons* params ?out-type (desugar-body body))))

;; Conform to pass convention.
;; NB: side-effecting the unused-table
;; desugar : ModuleStx UnusedTable AlphaEnv -> ModuleStx
(def (desugar module unused-table)
  (parameterize ((current-unused-table unused-table)
                 (current-verifications (make-hash-table)))
    (syntax-case module (@module)
      ((@module stmts ...)
       (retail-stx module (desugar-stmts (syntax->list #'(stmts ...))))))))

(export #t)

(import :std/format :std/iter
        :std/misc/list :std/srfi/1
        :std/misc/repr :clan/utils/debug ;; DEBUG
        <expander-runtime>
        (for-template :glow/compiler/syntax-context)
        :glow/compiler/syntax-context
        :glow/compiler/alpha-convert/fresh
        :glow/compiler/common)

;; Desugaring away these: @verifiably verify! @publicly defdata deftype and or
;; In the future, let users control desuraging of defdata and deftype with @deriving annotations (?)

;; desugar statements, accumulating desugared statements in reverse order
;; desugar-stmts : [Listof StmtStx] [Listof StmtStx] -> [Listof StmtStx]
;;(def (desugar-stmts stmts acc) (for/fold (acc acc) ((stmt stmts)) (desugar-stmt stmt acc)))

;; desugar-stmts : [Listof StmtStx] -> [Listof StmtStx]
(def (desugar-stmts stmts) (unsplice-stmts (map desugar-stmt stmts)))

;; desugar-stmt : StmtStx -> StmtStx
;; accumulate new reduced statements for the current unreduced statement,
;; in reversed order at the beginning of the accumulator acc
(def (desugar-stmt stx)
  (syntax-case stx (@ @interaction @verifiably @publicly deftype defdata publish! def)
    ((@interaction x s) (retail-stx stx [#'x (desugar-stmt #'s)]))
    ((@ p (@verifiably definition)) (retail-stx stx [#'p (desugar-verifiably stx #'p #'definition)]))
    ((@ p (@publicly definition)) (retail-stx stx [#'p (desugar-publicly stx #'p #'definition)]))
    ((@ p s) (identifier? #'p)
     (syntax-case #'s (splice)
       ((splice . body) (retail-stx #'s (map-in-order (λ (x) (desugar-stmt (restx x [#'@ #'p x])))
                                                      (syntax->list #'body))))
       (_ (retail-stx stx [#'p (desugar-stmt #'s)]))))
    ((defdata . _) (desugar-defdata stx))
    ((deftype . _) (desugar-deftype stx))
    ((publish! . _) stx)
    ((def . _) (desugar-def stx))
    (expr (desugar-expr stx))))

(def (variant->pattern variant)
  (syntax-case variant ()
    (constructor (identifier? #'constructor) #'constructor)
    ((constructor . args) (retail-stx variant (stx-map (λ (_) #'_) #'args)))))

(def (variants-to-nat variants)
  (for/collect ((i (in-naturals)) (variant (syntax->list variants)))
    [(variant->pattern variant) i]))

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
         (let ((x (mk-var 'x))
               (tag (mk-var 'tag)))
           (restx stx [#'λ [tag] [#'def x ': #'name [#'input #'name tag]] x])))
       (def toNat
         (let ((x (mk-var 'x))
               (cases (variants-to-nat #'(variant ...))))
           (restx stx [#'λ [[x ': #'name]] [#'switch x . cases]])))
       (def ofNat
         (let ((x (mk-var 'x))
               (cases (nat-to-variants #'(variant ...))))
           (and cases
                (restx stx [#'λ [[x ': #'name]] [#'switch x . cases]]))))
       (def rtvalue `(@record (input ,input)
                              (toNat ,toNat)
                              ,@(if ofNat `((ofNat ,ofNat)) '())))
       (retail-stx stx `(,#'spec ,@(syntax->list #'(variant ...)) with: ,rtvalue))))))

;; TODO: input, isA, JSON converters, EthBytes converters, etc.
;; desugar-deftype : Stx -> Stx
(def (desugar-deftype stx)
  stx)

(def current-verifications (make-parameter (make-hash-table)))

(def (computation-verification expr)
  ;; TODO: check that expr is a computation all made of verifiable elements
  expr)

(def (make-verification p var expr)
  (restx expr
    (syntax-case expr (sign)
      ((sign msg) [#'@app #'valid_signature var (computation-verification #'msg) p])
      (_ [#'@app #'= var (computation-verification expr)]))))

;; desugar-verifiably : Identifier Stx -> Stx
(def (desugar-verifiably stx p definition)
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
     (restx stx [#'splice (desugar-verifiably stx p definition)
                          #'(publish! name)
                          (desugar-verify [#'name])]))))

;; desugar-def : Stx -> Stx
(def (desugar-def stx)
  (syntax-case stx (:)
    ((def name : type expr)
     (retail-stx stx [#'name ': #'type (desugar-expr #'expr)]))
    ((def name expr)
     (retail-stx stx [#'name (desugar-expr #'expr)]))))

;; desugar-expr : Stx -> Stx
(def (desugar-expr stx)
  (syntax-case stx (@ ann @dot @tuple @record @list if and or block splice switch λ input digest sign require! assert! deposit! withdraw! verify! @app)
    ((@ _ _) (error 'desugar-expr "TODO: deal with @"))
    ((ann expr type) (retail-stx stx [(desugar-expr #'expr) #'type]))
    (x (identifier? #'x) stx)
    (lit (stx-atomic-literal? #'lit) #'lit)
    ((@dot e x) (identifier? #'x) (retail-stx stx [(desugar-expr #'e) #'x]))
    ((@tuple e ...) (desugar-keyword/sub-exprs stx))
    ((@list e ...) (desugar-keyword/sub-exprs stx))
    ((@record (x e) ...) (retail-stx stx (stx-map (lambda (x e) [x (desugar-expr e)]) #'(x ...) #'(e ...))))
    ((block b ...) (retail-stx stx (desugar-body (syntax->list #'(b ...)))))
    ((splice b ...) (retail-stx stx (desugar-body (syntax->list #'(b ...)))))
    ((if c t e) (desugar-keyword/sub-exprs stx))
    ((and e ...) (desugar-keyword/sub-exprs stx))
    ((or e ...) (desugar-keyword/sub-exprs stx))
    ((switch e swcase ...)
     (retail-stx stx (cons (desugar-expr #'e) (stx-map desugar-switch-case #'(swcase ...)))))
    ((λ . _) (desugar-lambda stx))
    ((input type tag) (retail-stx stx [#'type (desugar-expr #'tag)]))
    ((digest e ...) (desugar-keyword/sub-exprs stx))
    ((sign e ...) (desugar-keyword/sub-exprs stx))
    ((require! e) (desugar-keyword/sub-exprs stx))
    ((assert! e) (desugar-keyword/sub-exprs stx))
    ((deposit! e) (desugar-keyword/sub-exprs stx))
    ((withdraw! x e) (desugar-keyword/sub-exprs stx))
    ((verify! x ...) (desugar-verify #'(x ...)))
    ((@app a ...) (desugar-keyword/sub-exprs stx))))

(def (desugar-keyword/sub-exprs stx)
  (retail-stx stx (stx-map desugar-expr (stx-cdr stx))))

(def (desugar-body body)
  (if (null? body) body
      (append (desugar-stmts (butlast body)) [(desugar-expr (last body))])))

(def (desugar-switch-case stx)
  (syntax-case stx ()
    ((pat body ...)
     (restx stx (cons #'pat (desugar-body (syntax->list #'(body ...))))))))

(def (desugar-lambda stx)
  (syntax-case stx (:)
    ((_ params : out-type body ...)
     (retail-stx stx (cons* #'params ': #'out-type (desugar-body (syntax->list #'(body ...))))))
    ((_ params body ...)
     (retail-stx stx (cons* #'params (desugar-body (syntax->list #'(body ...))))))))

;; Conform to pass convention.
;; desugar : [Listof StmtStx] UnusedTable Env -> (values [Listof StmtStx] UnusedTable Env)
(def (desugar stmts unused-table env)
  (parameterize ((current-unused-table unused-table)
                 (current-verifications (make-hash-table)))
    (def desugared (desugar-stmts stmts))
    (values desugared unused-table env)))

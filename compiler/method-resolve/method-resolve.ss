(export method-resolve)

(import :gerbil/gambit/exact
        :gerbil/gambit/bytes
        :std/format
        :std/iter
        :std/misc/list
        :std/misc/repr
        :std/srfi/1
        :mukn/glow/compiler/syntax-context
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/alpha-convert/fresh
        :mukn/glow/compiler/typecheck/type
        :mukn/glow/compiler/typecheck/stx-prop
        :mukn/glow/compiler/common)

;; method-resolve : ModuleStx UnusedTable -> ModuleStx
(def (method-resolve stx unused-table)
  (parameterize ((current-unused-table unused-table)
                 (current-tysym-methods-table (make-tysym-methods-table)))
    (syntax-case stx (@module)
      ((@module stmts ...)
       (retail-stx stx (mr-stmts (syntax->list #'(stmts ...))))))))


;; A TysymMethodsTable is a [Hashof Symbol TysymMethodsEntry]
;; A TysymMethodsEntry is a (entry:tysym-methods Symbol [MaybeSymdictof Symbol])
(def (make-tysym-methods-table) (make-hash-table-eq))
(def current-tysym-methods-table (make-parameter (make-tysym-methods-table)))
(defstruct entry:tysym-methods (sym syms) transparent: #t)

;; set-tysym-methods-id! : Symbol Identifier -> Void
(def (set-tysym-methods-id! sym id)
  (hash-put! (current-tysym-methods-table) sym id))

;; get-tysym-methods-id : Symbol -> MaybeIdentifier
(def (get-tysym-methods-id sym)
  (hash-get (current-tysym-methods-table) sym))

;; mr-stmts : [Listof StmtStx] -> [Listof StmtStx]
(def (mr-stmts stmts) (append-map mr-stmt stmts))

;; mr-stmt : StmtStx -> [Listof StmtStx]
(def (mr-stmt stx)
  (syntax-case stx (@ splice : quote def deftype defdata publish! deposit!)
    ((splice s ...) (mr-stmts (syntax->list #'(s ...))))
    ((@ p s) (identifier? #'p)
     (for/collect ((s2 (mr-stmt #'s)))
       (retail-stx stx [#'p s2])))
    ((defdata . _) (mr-stmt-defdata stx))
    ((deftype . _) [stx])
    ((publish! . _) [stx])
    ((def . _) (mr-stmt-def stx))
    (expr [(mr-expr #'expr)])))

;; mr-stmt-defdata : StmtStx -> [Listof StmtStx]
(def (mr-stmt-defdata stx)
  (syntax-case stx ()
    ((_ spec variant ... with: rtvalue)
     (with-syntax* ((id (definition-lhs->id #'spec))
                    (methods-id (identifier-fresh #'id)))
       (def t (get-is-type #'id))
       (assert! (type:name? t) ["internal error: expected type:name, given" t])
       (set-tysym-methods-id! (type:name-sym t) #'methods-id)
       (cons (retail-stx stx (cons #'spec (syntax->list #'(variant ...))))
             (mr-stmt #'(def methods-id () rtvalue)))))))

;; mr-stmt-def : StmtStx -> [Listof StmtStx]
(def (mr-stmt-def stx)
  (syntax-case stx ()
    ((d x mt expr)
     [(retail-stx stx [#'x (mr-expr #'expr)])])))

;; mr-expr : ExprStx -> ExprStx
(def (mr-expr stx)
  (syntax-case stx (ann @make-interaction @tuple @list @record @dot @dot/type block splice if switch λ == input require! assert! deposit! withdraw! digest sign @app)
    (x (trivial-expr? #'x) stx)
    ((ann expr type) (mr-expr (track-has-typing-scheme stx #'expr)))
    ((@make-interaction parts params out-type body ...)
     (retail-stx stx (cons* #'parts (mr-params #'params) (mr-body (syntax->list #'(body ...))))))
    ((@tuple e ...) (mr-keyword/sub-exprs stx))
    ((@list e ...) (mr-keyword/sub-exprs stx))
    ((@record (x e) ...)
     (retail-stx
      stx
      (for/collect ((x (syntax->list #'(x ...))) (e (syntax->list #'(e ...))))
        [x (mr-expr e)])))
    ((@dot e x) (retail-stx stx [(mr-expr #'e) #'x]))
    ((@dot/type t x) (mr-expr-dot/type stx (get-is-type #'t) (stx-e #'x)))
    ((block b ...) (retail-stx stx (mr-body (syntax->list #'(b ...)))))
    ((splice b ...) (retail-stx stx (mr-body (syntax->list #'(b ...)))))
    ((switch e swcase ...)
     (retail-stx stx (cons (mr-expr #'e) (stx-map mr-switch-case #'(swcase ...)))))
    ((λ params out-type body ...)
     (retail-stx stx (cons* (mr-params #'params) (mr-body (syntax->list #'(body ...))))))
    ((== a b) (mr-keyword/sub-exprs stx))
    ((input type tag) (retail-stx stx [#'type (mr-expr #'tag)]))
    ((require! . _) (mr-keyword/sub-exprs stx))
    ((assert! . _) (mr-keyword/sub-exprs stx))
    ((deposit! . _) (mr-keyword/sub-exprs stx))
    ((withdraw! . _) (mr-keyword/sub-exprs stx))
    ((digest . _) (mr-keyword/sub-exprs stx))
    ((sign . _) (mr-keyword/sub-exprs stx))
    ((@app . _) (mr-keyword/sub-exprs stx))))

;; mr-params : ParamsStx -> [StxListof Id]
(def (mr-params stx)
  (restx1 stx (stx-map head-id stx)))

;; mr-body : [Listof StmtStx] -> [Listof StmtStx]
(def (mr-body body)
  (match body
    ([] [])
    ([e] [(mr-expr e)])
    ([a . bs] (append (mr-stmt a) (mr-body bs)))))

;; mr-expr-dot/type : ExprStx Type Symbol -> ExprStx
(def (mr-expr-dot/type stx type method-sym)
  (unless type (error 'mr-expr-dot/type "expected type is-type prop in" (syntax->datum stx)))
  (match type
    ((type:name type-sym)
     (def methods-id (get-tysym-methods-id type-sym))
     (cond (methods-id (restx stx ['@dot methods-id method-sym]))
           (else (error '@dot/type "type does not have methods"))))
    (_ (error '@dot/type "expected a type name, given" (type->sexpr type)))))

;; mr-keyword/sub-exprs : ExprStx -> ExprStx
(def (mr-keyword/sub-exprs stx)
  (retail-stx stx (stx-map mr-expr (stx-cdr stx))))

;; mr-switch-case : SwCaseStx -> SwCaseStx
(def (mr-switch-case stx)
  (retail-stx stx (mr-body (stx-cdr stx))))

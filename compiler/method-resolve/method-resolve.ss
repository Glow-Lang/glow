(export method-resolve read-type-table-file write-type-table type-table=?)

(import :gerbil/gambit/exact
        :gerbil/gambit/bytes
        :gerbil/gambit/exceptions
        :std/format
        :std/iter
        :std/misc/list
        :std/misc/repr
        :std/misc/hash
        :std/srfi/1
        (only-in :gerbil/gambit/ports output-port-readtable output-port-readtable-set!)
        (only-in :gerbil/gambit/readtables readtable-sharing-allowed?-set)
        :clan/base
        :clan/pure/dict/symdict
        :mukn/glow/compiler/syntax-context
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/alpha-convert/fresh
        :mukn/glow/compiler/typecheck/type
        :mukn/glow/compiler/typecheck/stx-prop
        :mukn/glow/compiler/common)

;; method-resolve : ModuleStx UnusedTable -> ModuleStx
(def (method-resolve stx unused-table)
  (def type-table (make-has-type-table))
  (parameterize ((current-unused-table unused-table)
                 (current-has-type-table type-table)
                 (current-tysym-methods-table (make-tysym-methods-table)))

    (syntax-case stx (@module)
      ((@module stmts ...)
       (values (retail-stx stx (mr-stmts (syntax->list #'(stmts ...))))
               type-table)))))


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
     (let ((expr* (mr-expr #'expr)))
       (set-has-type! #'x (get-has-type expr*))
       [(retail-stx stx [#'x expr*])]))))

;; get/compute-has-type : ExprStx -> (U Type #f)
(def (get/compute-has-type stx)
  (or (get-has-type stx)
      (resolve-type/scheme (get-has-typing-scheme stx))))

;; mr-expr : ExprStx -> ExprStx
(def (mr-expr stx)
  (def t (get/compute-has-type stx))
  (def stx* (mr-expr* stx))
  (set-has-type! stx* t)
  stx*)
(def (mr-expr* stx)
  (syntax-case stx (ann @make-interaction @tuple @list @record @dot @dot/type block splice if switch λ == input require! assert! deposit! withdraw! digest sign @app)
    (x (trivial-expr? #'x) stx)
    ((ann expr type) (mr-expr (track-has-typing-scheme stx #'expr)))
    ((@make-interaction parts params out-type body ...)
     (mr-expr-make-interaction stx))
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
    ((λ params out-type body ...) (mr-expr-lambda stx))
    ((== a b) (mr-keyword/sub-exprs stx))
    ((input type tag) (retail-stx stx [#'type (mr-expr #'tag)]))
    ((require! . _) (mr-keyword/sub-exprs stx))
    ((assert! . _) (mr-keyword/sub-exprs stx))
    ((deposit! . _) (mr-keyword/sub-exprs stx))
    ((withdraw! . _) (mr-keyword/sub-exprs stx))
    ((digest . _) (mr-keyword/sub-exprs stx))
    ((sign . _) (mr-keyword/sub-exprs stx))
    ((@app . _) (mr-keyword/sub-exprs stx))))

;; mr-expr-make-interaciton : ExpnStx -> ExprStx
(def (mr-expr-make-interaction stx)
  (syntax-case stx ()
    ((_ ((@list p ...)) params out-type body ...)
     (let ((xs (mr-params #'params)))
       (def t (get/compute-has-type stx))
       (def pxts (arg-types t))
       (for-each set-has-type!
                 (append (syntax->list #'(p ...))
                         (syntax->list xs))
                 pxts)
       (def r (retail-stx stx (cons* #'((@list p ...)) xs (mr-body (syntax->list #'(body ...))))))
       (set-has-type! r t)
       r))))

;; mr-expr-lambda : ExprStx -> ExprStx
(def (mr-expr-lambda stx)
  (syntax-case stx ()
    ((_ params out-type body ...)
     (let ((xs (mr-params #'params)))
       (def t (get/compute-has-type stx))
       (def xts (arg-types t))
       (for-each set-has-type! (syntax->list xs) xts)
       (def r (retail-stx stx (cons* xs (mr-body (syntax->list #'(body ...))))))
       (set-has-type! r t)
       r))))

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

;; --------------------------------------------------------

;; resolve-type/scheme : TypingScheme -> Type
(def (resolve-type/scheme ts)
  (def ts* (typing-scheme-simplify ts))
  (match ts*
    ((typing-scheme (? symdict-empty?) t) t)
    ((typing-scheme menv t)
     (def xs (symdict-keys menv))
     (defvalues (var-xs other-xs)
       (partition (lambda (x) (type:var? (symdict-ref menv x)))
                  xs))
     (def other-ts
       (map (lambda (x) (symdict-ref menv x)) other-xs))
     (cond
       ((andmap type-closed? other-ts)
        (type-subst (list->symdict
                     (for/collect ((x var-xs))
                       (cons (type:var-sym (symdict-ref menv x))
                             (get-has-type x))))
                    t))
       ((null? (type-vars t)) t)
       (else
        (displayln "resolve-type/scheme: TODO")
        (print-typing-scheme ts*)
        (newline)
        t)))))

;; arg-types : Type -> [Listof Type]
(def (arg-types t)
  (match t
    ((type:arrow args _) args)
    ((ntype:intersection [_ ... (type:arrow args _) . _]) args)
    ((ptype:union [(type:arrow argss _) ...])
     (apply map (compose types-meet list) argss))
    (_ [])))

(def (hash->repr-sexpr h k->s v->s)
  (cons 'hash
        (for/collect ((p (hash->list/sort h sexpr<?)))
          (with (([k . v] p))
            [(k->s k) (v->s v)]))))

(def (repr-sexpr->hash s s->k s->v)
  (match s
    ((cons 'hash ents)
     (def h (make-hash-table))
     (for ((ent ents))
       (with (([ks vs] ent))
         (def k (s->k ks))
         (def v (s->v vs))
         (hash-put! h k v)))
     h)))

;; type-table->repr-sexpr
(def (type-table->repr-sexpr tbl)
  (and tbl (hash->repr-sexpr tbl identity
             (lambda (t) (and t (type->repr-sexpr t))))))
;; repr-sexpr->type-table
(def (repr-sexpr->type-table s)
  (and s (repr-sexpr->hash s identity
           (lambda (t) (and t (repr-sexpr->type t))))))

(def (read-type-table-file file)
  (with-catch
   (lambda (e) (display-exception e) #f)
   (lambda ()
     (match (read-syntax-from-file file)
       ([s] (repr-sexpr->type-table (syntax->datum s)))
       (_ (printf "read-type-table-file: expected a single symdict sexpr\n") #f)))))

(def (write-type-table tbl (port (current-output-port)))
  (when tbl
    (output-port-readtable-set!
     port
     (readtable-sharing-allowed?-set (output-port-readtable port) #f))
    (fprintf port "~y" (type-table->repr-sexpr tbl))))

(def (type-table=? a b)
  (and a b (equal? (type-table->repr-sexpr a) (type-table->repr-sexpr b))))

;; symbols < bools < real-numbers < strings < null < pairs < others
(def (sexpr<? a b)
  (cond
    ((and (symbol? a) (symbol? b)) (symbol<? a b))
    ((symbol? a) #t)
    ((symbol? b) #f)
    ((and (boolean? a) (boolean? b)) (and (not a) b))
    ((boolean? a) #t)
    ((boolean? b) #f)
    ((and (real? a) (real? b)) (< a b))
    ((real? a) #t)
    ((real? b) #f)
    ((and (string? a) (string? b)) (string<? a b))
    ((string? a) #t)
    ((string? b) #f)
    ((and (null? a) (null? b)) #f)
    ((null? a) #t)
    ((null? b) #f)
    ((and (pair? a) (pair? b))
     (if (equal? (car a) (car b))
         (sexpr<? (cdr a) (cdr b))
         (sexpr<? (car a) (car b))))
    ((pair? a) #t)
    ((pair? b) #f)
    (else (string<? (format "~s" a) (format "~s" b)))))


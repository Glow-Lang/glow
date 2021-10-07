(export method-resolve
        read-type-table-file
        write-type-table
        type-table=?
        current-tysym-methods-table
        get-tysym-methods-id
        read-tysym-methods-table-file
        write-tysym-methods-table
        tysym-methods-table=?
        current-methods-id-back-table
        get-methods-id-back
        read-methods-id-back-table-file
        write-methods-id-back-table
        methods-id-back-table=?)


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

;; method-resolve : ModuleStx UnusedTable -> (values ModuleStx TypeTable TysymMethodsTable MethodsIdBackTable)
(def (method-resolve stx unused-table)
  (def type-table (make-has-type-table))
  (def tysym-methods-table (make-tysym-methods-table))
  (def methods-id-back-table (make-methods-id-back-table))
  (parameterize ((current-unused-table unused-table)
                 (current-has-type-table type-table)
                 (current-tysym-methods-table tysym-methods-table)
                 (current-methods-id-back-table methods-id-back-table))

    (syntax-case stx (@module)
      ((@module stmts ...)
       (values (retail-stx stx (mr-stmts (syntax->list #'(stmts ...))))
               type-table
               tysym-methods-table
               methods-id-back-table)))))


;; A TysymMethodsTable is a [Hashof Symbol TysymMethodsEntry]
;; maps the symbols in type:name to symbols for runtime method-dictionaries
;; A TysymMethodsEntry is a (entry:tysym-methods Symbol [MaybeSymdictof Symbol])
(def (make-tysym-methods-table) (make-hash-table-eq))
(def current-tysym-methods-table (make-parameter (make-tysym-methods-table)))
(defstruct entry:tysym-methods (sym syms) transparent: #t)

;; A MethodsIdBackTable is a [Hashof Symbol Symbol]
;; maps the symbols for runtime method-dictionaries to symbols for types
(def (make-methods-id-back-table) (make-hash-table-eq))
(def current-methods-id-back-table (make-parameter (make-methods-id-back-table)))

;; set-tysym-methods-id! : Symbol Identifier -> Void
(def (set-tysym-methods-id! sym id)
  (hash-put! (current-tysym-methods-table) sym id))

;; get-tysym-methods-id : Symbol -> MaybeIdentifier
(def (get-tysym-methods-id sym)
  (hash-get (current-tysym-methods-table) sym))

;; set-methods-id-back! : Identifier Identifier -> Void
(def (set-methods-id-back! m t)
  (hash-put! (current-methods-id-back-table) (syntax->datum m) (syntax->datum t)))

;; get-methods-id-back : Identifier -> (U Symbol #f)
(def (get-methods-id-back m)
  (hash-get (current-methods-id-back-table) (syntax->datum m)))

;; mr-stmts : [Listof StmtStx] -> [Listof StmtStx]
(def (mr-stmts stmts) (append-map mr-stmt stmts))

;; mr-stmt : StmtStx -> [Listof StmtStx]
(def (mr-stmt stx)
  (syntax-case stx (@ splice @debug-label : quote def deftype defdata publish! deposit!)
    ((splice s ...) (mr-stmts (syntax->list #'(s ...))))
    ((@debug-label . _) [stx])
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
       (set-methods-id-back! #'methods-id #'id)
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
      (let ((ts (get-has-typing-scheme stx)))
        (unless ts (raise-syntax-error #f "typing scheme not found" stx))
        (resolve-type/scheme ts))))

;; mr-expr : ExprStx -> ExprStx
(def (mr-expr stx)
  (def t (get/compute-has-type stx))
  (def stx* (mr-expr* stx))
  (set-has-type! stx* t)
  stx*)
(def (mr-expr* stx)
  (syntax-case stx (ann @make-interaction @tuple @list @record @dot @dot/type block splice if switch λ == input require! assert! deposit! withdraw! digest sign @app-ctor @app)
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
    ((deposit! . _) (mr-deposit-withdraw stx))
    ((withdraw! . _) (mr-deposit-withdraw stx))
    ((digest . _) (mr-keyword/sub-exprs stx))
    ((sign . _) (mr-keyword/sub-exprs stx))
    ((@app-ctor . _) (mr-keyword/sub-exprs stx))
    ((@app . _) (mr-keyword/sub-exprs stx))))

;; mr-expr-make-interaction : ExprStx -> ExprStx
(def (mr-expr-make-interaction stx)
  (syntax-case stx (@record @list participants assets)
    ((_ ((@record (participants (@list p ...)) (assets (@list a ...))))
        params
        out-type
        body
        ...)
     (let ((xs (mr-params #'params)))
       (def t (get/compute-has-type stx))
       (def pxts (arg-types t))
       (for-each set-has-type!
                 (append (syntax->list #'(p ...))
                         (syntax->list xs))
                 pxts)
       (def r
         (retail-stx stx
           (cons* #'((@record (participants (@list p ...)) (assets (@list a ...))))
                  xs
                  (mr-body (syntax->list #'(body ...))))))
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
(def (mr-body body) (mr-stmts body))

;; mr-expr-dot/type : ExprStx Type Symbol -> ExprStx
(def (mr-expr-dot/type stx type method-sym)
  (unless type (error 'mr-expr-dot/type "expected type is-type prop in" (syntax->datum stx)))
  (match type
    ((type:name type-sym)
     (def methods-id (get-tysym-methods-id type-sym))
     (cond (methods-id (restx stx ['@dot methods-id method-sym]))
           (else (error '@dot/type "type does not have methods"))))
    (_ (error '@dot/type "expected a type name, given" (type->sexpr type)))))

;; mr-deposit-withdraw : ExprStx -> ExprStx
(def (mr-deposit-withdraw stx)
  (syntax-case stx (@record)
    ((_ lbl p (@record (x e) ...))
     (retail-stx stx
       [#'lbl (mr-expr #'p)
        (cons '@record (stx-map list #'(x ...) (stx-map mr-expr #'(e ...))))]))))

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
     (def other-t-vars
       (flatten1 (map type-vars other-ts)))
     (def t-vars (type-vars t))
     (cond
       ((andmap (lambda (v) (not (member v other-t-vars))) t-vars)
        (type-subst (list->symdict
                     (for/collect ((x var-xs))
                       (cons (type:var-sym (symdict-ref menv x))
                             (get-has-type x))))
                    t))
       (else
        (printf "resolve-type/scheme: TODO ~a\n"
                (filter (cut member <> other-t-vars) t-vars))
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

;; type-table->repr-sexpr
(def type-table->repr-sexpr
  (hash->repr-sexpr
    identity
    (lambda (t) (and t (type->repr-sexpr t)))
    sexpr<?))
;; repr-sexpr->type-table
(def repr-sexpr->type-table
  (repr-sexpr->hash
    identity
    (lambda (t) (and t (repr-sexpr->type t)))))

(def (read-type-table-file file)
  ;; TODO: move error handling to `run-pass` or `run-passes` in multipass.ss
  (with-catch
   (lambda (e) (display-exception e) #f)
   (lambda ()
     (match (read-syntax-from-file file)
       ([s] (repr-sexpr->type-table (syntax->datum s)))
       (_ (printf "read-type-table-file: expected a single hash sexpr\n") #f)))))

(def (write-type-table tbl (port (current-output-port)))
  (when tbl
    (output-port-readtable-set!
     port
     (readtable-sharing-allowed?-set (output-port-readtable port) #f))
    (fprintf port "~y" (type-table->repr-sexpr tbl))))

(def (type-table=? a b)
  (and a b (equal? (type-table->repr-sexpr a) (type-table->repr-sexpr b))))

;; tysym-methods-table->repr-sexpr
(def tysym-methods-table->repr-sexpr
  (hash->repr-sexpr
    identity
    (lambda (t) (and t (symbol->repr-sexpr (syntax->datum t))))
    symbol<?))
;; repr-sexpr->tysym-methods-table
(def repr-sexpr->tysym-methods-table
  (repr-sexpr->hash
    identity
    (lambda (t) (and t (repr-sexpr->symbol t)))))

(def (read-tysym-methods-table-file file)
  ;; TODO: move error handling to `run-pass` or `run-passes` in multipass.ss
  (with-catch
   (lambda (e) (display-exception e) #f)
   (lambda ()
     (match (read-syntax-from-file file)
       ([s] (repr-sexpr->tysym-methods-table (syntax->datum s)))
       (_ (printf "read-tysym-methods-table-file: expected a single hash sexpr\n") #f)))))

(def (write-tysym-methods-table tbl (port (current-output-port)))
  (when tbl
    (output-port-readtable-set!
     port
     (readtable-sharing-allowed?-set (output-port-readtable port) #f))
    (fprintf port "~y" (tysym-methods-table->repr-sexpr tbl))))

(def (tysym-methods-table=? a b)
  (and a b (equal? (tysym-methods-table->repr-sexpr a) (tysym-methods-table->repr-sexpr b))))

;; methods-id-back-table->repr-sexpr
(def methods-id-back-table->repr-sexpr
  (hash->repr-sexpr identity symbol->repr-sexpr symbol<?))
;; repr-sexpr->methods-id-back-table
(def repr-sexpr->methods-id-back-table
  (repr-sexpr->hash identity repr-sexpr->symbol))

(def (read-methods-id-back-table-file file)
  ;; TODO: move error handling to `run-pass` or `run-passes` in multipass.ss
  (with-catch
   (lambda (e) (display-exception e) #f)
   (lambda ()
     (match (read-syntax-from-file file)
       ([s] (repr-sexpr->methods-id-back-table (syntax->datum s)))
       (_ (printf "read-methods-id-back-table-file: expected a single hash sexpr\n") #f)))))

(def (write-methods-id-back-table tbl (port (current-output-port)))
  (when tbl
    (output-port-readtable-set!
     port
     (readtable-sharing-allowed?-set (output-port-readtable port) #f))
    (fprintf port "~y" (methods-id-back-table->repr-sexpr tbl))))

(def (methods-id-back-table=? a b)
  (and a b (equal? (methods-id-back-table->repr-sexpr a) (methods-id-back-table->repr-sexpr b))))


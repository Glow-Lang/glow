(export parse)
(import :drewc/smug
        :mukn/glow/compiler/lexical
        :mukn/glow/compiler/expressions
        (only-in :mukn/glow/compiler/alpha-convert/alpha-convert keyword-syms))

;; parse : InputPort -> SExpr
(def (parse in)
  (def lang-line (read-line in))
  (unless (string=? lang-line "#lang glow")
    (error 'parse "expected `#lang glow` on first line"))
  (def str (read-line in #f))
  (def lex (lexify str))
  (def fsl (run FunctionStatementList lex))
  (fsl->module fsl))

;; fsl->module : FunctionStatementList -> SExpr
(def (fsl->module fsl)
  (cond
    ((not fsl) '(@module))
    (else `(@module ,@(map stat->sexpr fsl)))))

;; id->sexpr : Identifier -> SExpr
(def (id->sexpr x)
  (match x
    ((identifier name) (string->symbol name))))

;; lit->sexpr : Literal -> SExpr
(def (lit->sexpr x)
  (match x
    ((null-literal value) ('TODO))
    ((boolean-literal value) ('TODO value))
    ((numeric-literal value) ('TODO value))
    ((string-literal value) ('TODO value))))

;; type->sexpr : Type -> SExpr
(def (type->sexpr t)
  (match t
    ((annotated-type attr type) `(@ ,(attr->sexpr attr) ,(type->sexpr type)))
    ((type-name id) (string->symbol id))
    ((type-var id) `',(string->symbol id))
    ((type-tuple args) `(@tuple ,@(map type->sexpr args)))
    ((type-record entries)
     `(@record ,@(map (match <> ([k . v] [(string->symbol k) (type->sexpr v)])) entries)))))

;; expr->sexpr : Expression -> SExpr
(def (expr->sexpr x)
  (match x
    ((identifier _) (id->sexpr x))
    ((literal _) (lit->sexpr x))
    ((operator lhs op rhs) ('TODO))
    ((unary-expression op expr) ('TODO))
    ; parens
    ((bracket-expression exps) `(@tuple ,@(map expr->sexpr exps)))
    ; square brackets
    ((compound-expression exprs) `(@list ,@(map expr->sexpr exprs)))
    ((call-expression fn (arguments args))
     (let ((f (expr->sexpr fn)))
       (if (and (symbol? f) (memq f keyword-syms))
           `(,f ,@(map expr->sexpr args))
           `(@app ,f ,@(map expr->sexpr args)))))
    ((require-expression exp) `(require! ,(expr->sexpr exp)))
    ((assert-expression exp) `(assert! ,(expr->sexpr exp)))
    ((deposit-expression id exp) `(deposit! ,(id->sexpr id) ,(expr->sexpr exp)))
    ((withdraw-expression id exp) `(withdraw! ,(id->sexpr id) ,(expr->sexpr exp)))
    ((annotated-expression attr expr) `(@ ,(attr->sexpr attr) ,(expr->sexpr expr)))
    ((dot-expression expr id) `(@dot ,(expr->sexpr expr) ,(id->sexpr id)))
    ((type-expression expr typ) `(ann ,(expr->sexpr expr) ,(type->sexpr typ)))
    ((record-expr entries)
     `(@record ,@(map (match <> ((record-expr-entry id exp) [(id->sexpr id) (expr->sexpr exp)])) entries)))
    ((block-expression body) `(block ,@(body->sexprs body)))
    ((body [] expr)          (expr->sexpr expr))
    ((body _ _)              `(block ,@(body->sexprs x)))
    ((if-expression c t e) `(if ,(expr->sexpr c) ,(expr->sexpr t) ,(expr->sexpr e)))
    ((switch expr cases) `(switch ,(expr->sexpr expr) ,@(map case->sexpr cases)))))

;; body->sexprs : Body -> [Listof SExpr]
(def (body->sexprs b)
  (match b
    ((body stmts #f)         (map stat->sexpr stmts))
    ((body stmts expr)       (append (map stat->sexpr stmts) [(expr->sexpr expr)]))
    ((block-expression body) (body->sexprs body))
    (expr                    [(expr->sexpr expr)])))

;; case->sexpr : Case -> SExpr
(def (case->sexpr c)
  (match c
    ((case pat body) (cons (pat->sexpr pat) (body->sexprs body)))))

;; stat->sexpr : Statement -> SExpr
(def (stat->sexpr s)
  (match s
    ((publish-statement id expr) `(publish! ,(id->sexpr id) ,(expr->sexpr expr)))
    ((verify-statement id)       `(verify! ,(id->sexpr id)))
    ((type-declaration id typarams typ)
     (cond
       (typarams `(deftype (,(id->sexpr id) ,@(map id->sexpr typarams)) ,(type->sexpr typ)))
       (else     `(deftype ,(id->sexpr id) ,(type->sexpr typ)))))
    ((dataAssignmentStatement id typarams variants)
     (cond
       (typarams `(defdata (,(id->sexpr id) ,@(map id->sexpr typarams)) ,@(map variant->sexpr variants)))
       (else     `(defdata ,(id->sexpr id) ,@(map variant->sexpr variants)))))
    ((expression-statement expr) (expr->sexpr expr))
    ((assignment-statement id type expr)
     (cond
       (type `(def ,(id->sexpr id) : ,(type->sexpr type) ,(expr->sexpr expr)))
       (else `(def ,(id->sexpr id) ,(expr->sexpr expr)))))
    ((function-declaration id params type body)
     (cond
       (type `(def ,(id->sexpr id) (λ ,(map param->sexpr params) : ,(type->sexpr type) ,@(body->sexprs body))))
       (else `(def ,(id->sexpr id) (λ ,(map param->sexpr params) ,@(body->sexprs body))))))
    ((annotationStatement attr stat) `(@ ,(attr->sexpr attr) ,(stat->sexpr stat)))))

;; param->sexpr : Param -> SExpr
(def (param->sexpr p)
  (match p
    ((param-data id #f) (id->sexpr id))
    ((param-data id typ) `(,(id->sexpr id) : ,(type->sexpr typ)))))

;; variant->sexpr : Variant -> SExpr
(def (variant->sexpr v)
  (match v
    ([id . types] `(,(id->sexpr id) ,@(map type->sexpr types)))
    (id (id->sexpr id))))

;; attr->sexpr : Attribute -> SExpr
(def (attr->sexpr a)
  (match a
    ((attribute id #f) (id->sexpr id))
    ((attribute id args) `(,(id->sexpr id) ,@(map expr->sexpr args)))))

;; pat->sexpr : Pat -> SExpr
(def (pat->sexpr p)
  (match p))

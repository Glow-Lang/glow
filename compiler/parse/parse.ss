(export parse)
(import :drewc/smug
        :mukn/glow/compiler/parse/lexical
        :mukn/glow/compiler/parse/expressions
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

;; op->sexpr : Op -> SExpr
(def (op->sexpr x)
  (def str
    (cond ((string? x) x)
          ((char? x) (string x))
          (else (error 'op->sexpr "unknown operation:" x))))
  (match str
    ("%" 'mod)
    ("&&" 'and)
    ("||" 'or)
    ("!" 'not)
    ("|||" 'bitwise-or)
    ("&&&" 'bitwise-and)
    ("^^^" 'bitwise-xor)
    ("~~~" 'bitwise-not)
    ("<<"  'bitwise-shift-right)
    (">>"  'bitwise-shift-left)
    (_ (string->symbol str))))

;; lit->sexpr : Literal -> SExpr
(def (lit->sexpr x)
  (match x
    ((boolean-literal value) value)
    ((numeric-literal value) (string->number value))
    ((string-literal ['DoubleQuoteStringLiteral str]) str)))

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
    ((operator lhs op rhs) `(,(op->sexpr op) ,(expr->sexpr lhs) ,(expr->sexpr rhs)))
    ((unary-expression op expr) `(,(op->sexpr op) ,(expr->sexpr expr)))
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
     `(@record ,@(map (match <> ([k . v] [(id->sexpr k) (expr->sexpr v)])) entries)))
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
    ((publish-statement p-id x-ids) `(publish! ,(id->sexpr p-id) ,@(map id->sexpr x-ids)))
    ((verify-statement ids)       `(verify! ,@(map id->sexpr ids)))
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
  (match p
    ((annotated-pattern attr pat) `(@ ,(attr->sexpr attr) ,(pat->sexpr pat)))
    ((type-pattern pat typ) `(ann ,(pat->sexpr pat) ,(type->sexpr typ)))
    ((pattern-id id) (id->sexpr id))
    ((pattern-blank) '_)
    ((pattern-lit lit) (lit->sexpr lit))
    ((pattern-tuple args) `(@tuple ,@(map pat->sexpr args)))
    ((pattern-or pats) `(@or-pat ,@(map pat->sexpr pats)))
    ((pattern-list args) `(@list ,@(map pat->sexpr args)))
    ((pattern-record entries)
     `(@record ,@(map (match <> ([k . v] [(id->sexpr k) (pat->sexpr v)])) entries)))))

(import :drewc/smug :mukn/glow/compiler/parse/lexical :std/iter :std/misc/list)

(export #t)

(defstruct expression ())
(defstruct statement ())

(def (parse-operator side op)
  (.let* ((lhs side) (pop? (.or (match-token-value? op) #f)))
    (if (equal? pop? #f) (return (values lhs #f #f))
        (.let* (rhs side) (return (values lhs pop? rhs))))))

(defstruct (operator expression) (lhs op rhs) transparent: #t)
(defrules Operator ()
  ((_ maker side op)
   (.begin
     #t
     (.let* ((values lhs pop rhs) (parse-operator side op))
       (return (if rhs (maker lhs pop rhs) lhs))))))

(defstruct (identifier expression) (name) transparent: #t)
(def Identifier
  (.begin (peek (match-token-type? 'IdentifierName))
          (peek (match-token-value? (.not ReservedWord)))
          (.let* (t (item))
            (return (identifier (get-token-value t))))))

(defstruct (literal expression) (value) transparent: #t)
(defstruct (boolean-literal literal) () transparent: #t)
(defstruct (numeric-literal literal) () transparent: #t)
(defstruct (string-literal literal) () transparent: #t)

(def Literal
  (.or
       (.let* (l BooleanLiteral) (return (boolean-literal (get-token-value l))))
       (.let* (t (.begin (peek (match-token-type? 'StringLiteral)) (item)))
         (return (string-literal (get-token-value t))))
       (.let* (t (.begin (peek (match-token-type? 'IntegerLiteral)) (item)))
         (return (numeric-literal (get-token-value t))))))

(defstruct (bracket-expression expression) (exps) transparent: #t)
(def BracketExpression
  (.begin (match-token-value? #\()
    (.let* (
           (exps (sepby1 Expression  (match-token-value? #\,)))
            (_(match-token-value? #\) )))
          (return (if (length=n? exps 1) (car exps) (bracket-expression exps))))))

(def TightExpression
  (.begin #t (.or BracketExpression CompoundExpression RecordExpr BlockExpression Identifier Literal
                  ;; NOTE: this .begin must be after Literal
                  ;; TODO: figure out why, and when fixed move it back to being part of BracketExpression
                  (.begin (match-token-value? #\() (match-token-value? #\) ) (return (bracket-expression []))))))

(def PrimaryExpression
  ; CallExpression | DotExpression | TightExpression
  (.let* ((a TightExpression)
          (bs (many ArgumentsOrDot)))
    (for/fold (a a) ((b bs))
      (cond ((arguments? b) (call-expression a b))
            (else (dot-expression a b))))))

(defstruct (arguments expression) (list) transparent: #t)
(def Arguments
  (.let* ((_ (match-token-value? #\())
          (empty? (.or (match-token-value? #\)) #f))
          (args (if empty? [] ArgumentList))
          (_(if (equal? empty? #f)  (match-token-value? #\)) )))
    (return (arguments args))))

(def ArgumentList (.begin #t (sepby1 Expression (match-token-value? #\,))))

(def ArgumentsOrDot
  (.or Arguments (.begin (match-token-value? #\.) Identifier)))

(defstruct (unary-expression expression) (op expression) transparent: #t)
(def UnaryExpression
    (.let* ((op (.or (match-token-value? (.or "+" "-" "~~~" "!")) #f))
            (exp TypedOrNonTypedPrimaryExpression))
              (if op  (return (unary-expression op exp)) exp)))

(defstruct (exponentiation-expression operator) () transparent: #t)
(def ExponentiationExpression
  (Operator exponentiation-expression UnaryExpression "**"))

(defstruct (multiplicative-expression operator) () transparent: #t)
(def MultiplicativeExpression
  (.let* ((g (Operator multiplicative-expression ExponentiationExpression MultiplicativeOperator))
           (e (.or (match-token-value? #\*) (match-token-value? #\/) (match-token-value? #\%)  #f))
           (right (if e (.let* (a MultiplicativeExpression)(multiplicative-expression g e a)) g))) right))


(def MultiplicativeOperator (.or #\* #\/ #\%))

(defstruct (additive-expression operator) () transparent: #t)
(def AdditiveExpression
  (.let* ((g (Operator additive-expression MultiplicativeExpression (.or "+" "-")))
           (e (.or (match-token-value? "+") (match-token-value? "-") #f))
           (right (if e (.let* (a AdditiveExpression)(additive-expression g e a)) g))) right))


(defstruct (shift-expression operator) () transparent: #t)
(def ShiftExpression
  (Operator shift-expression AdditiveExpression (.or  ">>" "<<")))


(defstruct (relational-expression operator) () transparent: #t)
(def RelationalExpression
  (Operator relational-expression ShiftExpression
            (.or "<=" ">=" "<" ">")))


(defstruct (equality-expression operator) () transparent: #t)
(def EqualityExpression
  (.or (Operator equality-expression RelationalExpression "==")
       (Operator equality-expression RelationalExpression "!=")))


(defstruct (bitwise-and-expression operator) () transparent: #t)
(def BitwiseANDExpression
  (Operator bitwise-and-expression EqualityExpression "&&&"))

(defstruct (bitwise-xor-expression operator) () transparent: #t)
(def BitwiseXORExpression
  (Operator bitwise-xor-expression BitwiseANDExpression "^^^"))

(defstruct (bitwise-or-expression operator) () transparent: #t)
(def BitwiseORExpression
  (Operator bitwise-or-expression BitwiseXORExpression "|||"))

(defstruct (logical-and-expression operator) () transparent: #t)
(def LogicalANDExpression
  (Operator logical-and-expression BitwiseORExpression "&&"))

(defstruct (logical-or-expression operator) () transparent: #t)
(def LogicalORExpression
  (Operator logical-or-expression LogicalANDExpression "||"))

(def ConditionalExpression (.begin #t LogicalORExpression))
(def ArithmeticExpression (.begin #t ConditionalExpression))


(defstruct attribute (id arguments)  transparent: #t)

(def Attribute
  (.or (.let* ((id Identifier)
               (_(match-token-value? #\( ) )
               (arg-expr Expression) ; TODO: multiple Arguments
               (_(match-token-value? #\) )) )
         (attribute id [arg-expr]))
       (.let* (id Identifier) (attribute id #f))))


(defstruct (call-expression expression) (function arguments)  transparent: #t)

(defstruct (require-expression expression) (exp) transparent: #t)
(def RequireExpression
    (.let* ((exp ConditionalExpression))
          (return (require-expression exp))))

(defstruct (assert-expression expression) (exp) transparent: #t)
(def AssertExpression
    (.let* ((exp ConditionalExpression))
          (return (assert-expression exp))))

(defstruct (deposit-expression expression) (id exp) transparent: #t)
(def DepositExpression
    (.let* ((id Identifier) (_(match-token-value? "->")) (exp Expression))
          (return (deposit-expression id exp))))

(defstruct (withdraw-expression expression) (id exp) transparent: #t)
(def WithdrawExpression
    (.let* ((id Identifier)
            (_(match-token-value? "<-"))
            (exp ArithmeticExpression))
          (return (withdraw-expression id exp))))

(defstruct (annotated-expression expression) (attr  expr) transparent: #t)
(def AnnotatedExpression
  (.begin (match-token-value? #\@)
    (.let* ((attr Attribute) (exp PrimaryExpression))
      (return (annotated-expression attr exp)))))

(defstruct (dot-expression expression) (expr id) transparent: #t)

(defstruct (compound-expression expression) (expressions) transparent: #t)
(def CompoundExpression
  (.begin (match-token-value? #\[)
    (.let*  ((ce (sepby1  Expression (match-token-value? #\,)))
              (_(match-token-value? #\])))
      (return (compound-expression ce)))))

(def IdentifierTokenName
  (.begin (peek (match-token-type? 'IdentifierName))
          (peek (match-token-value? (.not ReservedWord)))
          (.let* (t (item)) (get-token-value t))))

(defstruct type () transparent: #t)
(defstruct (annotated-type type) (attr type) transparent: #t)
(defstruct (type-name type) (id) transparent: #t)
(defstruct (type-var type) (id) transparent: #t)
(defstruct (type-tuple type) (args) transparent: #t)
(defstruct (type-record type) (field-args) transparent: #t)
(def BaseType
    (.or
      (.let* (id IdentifierTokenName) (return (type-name id)))
      (.begin (match-token-value? #\')
        (.let* ( (id IdentifierTokenName)) (return (type-var id))))))

(def AnnotatedType
   (.begin (match-token-value? #\@)
        (.let* ((attr Attribute) (typ BaseType)) (return (annotated-type attr typ)))))

(def BracketedType
  (.begin (match-token-value? #\( )
        (.let* ((typs (sepby1 BaseType  (match-token-value? #\,))) (_(match-token-value? #\))  ))
                 (return (if (length=n? typs 1) (car typs) (type-tuple typs))))))
(def RecordType
      (.begin (match-token-value? #\{)
        (.let* ((entries  RecordTypeEntries)  (_(match-token-value? #\}))) (return (type-record entries)))))

(def RecordTypeEntry
    (.let* ((id Identifier) (_(match-token-value? #\:)) (typ Type)) (cons id typ)))

(def RecordTypeEntries
    (.let* (entries (sepby1 RecordTypeEntry (match-token-value? #\,)))
        (return entries)))

(defstruct (type-expression expression) (expr typ) transparent: #t)
(def TypedOrNonTypedPrimaryExpression
  (.let* ((exp PrimaryExpression) (typ? (.or (.begin (match-token-value? #\:)  BaseType) #f)))
      (if (equal? typ? #f) exp  (return (type-expression exp typ?)))))

(def Type
  (.or RecordType BracketedType  AnnotatedType BaseType))

(def Variant
    (.let* ((name Identifier)
            (marker? (.or (match-token-value? #\( ) #f)))
        (if (equal? marker? #f)
            name
            (.let* ((typs (sepby1 Type  (match-token-value? #\,)))
                    (_(match-token-value? #\) )) )
              (cons name typs)))))

(def Variants
  (.begin (.or (match-token-value? #\|) #t)
          (sepby1 Variant (match-token-value? #\|))))

(defstruct (record-expr expression) (entries) transparent: #t)
(def RecordExpr
  (.begin (match-token-value? #\{)
          (.let* ((records RecordExprEntries)
                  (_(match-token-value? #\})))
            (return (record-expr records)))))

(def RecordExprEntries
  (sepby1 (.let* ((id Identifier)
                  (_(match-token-value? #\:))
                  (expr ConditionalExpression))
            (return (cons id expr)) )
          (match-token-value? #\,)))

(defstruct pattern () transparent: #t)
(defstruct (annotated-pattern pattern) (attr pat)  transparent: #t)
(defstruct (type-pattern pattern) (pat typ) transparent: #t)
;; pattern-id is used for both pattern-variables and id data-constructors
(defstruct (pattern-id pattern) (id) transparent: #t)
(defstruct (pattern-blank pattern) () transparent: #t)
(defstruct (pattern-lit pattern) (lit) transparent: #t)
(defstruct (pattern-tuple pattern) (args) transparent: #t)
(defstruct (pattern-or pattern) (pats) transparent: #t)
(defstruct (pattern-list pattern) (args) transparent: #t)
(defstruct (pattern-record pattern) (entries) transparent: #t)
(defstruct (pattern-app-ctor pattern) (id args) transparent: #t)
(def BasePattern
    (.or
        (.let* (id Identifier) (return (pattern-id id)))
        (.let* (_(match-token-value? #\_)) (return (pattern-blank)))
        (.let* (lit Literal) (return (pattern-lit lit)))))

(def BracketedPattern
  (.begin  (match-token-value? #\()
           (.let* ((rst (sepby1 Pattern (match-token-value? #\,))) (_(match-token-value? #\))))
                (return (if (length=n? rst 1) (car rst) (pattern-tuple rst))))))

(def BlockPattern
  (.begin (match-token-value? #\[)
          (.let* ((rst (sepby1 Pattern (match-token-value? #\,))) (_(match-token-value? #\])))
                (return (pattern-list rst)))))

(def AnnotatedPattern
  (.begin (match-token-value? #\@)
          (.let* ((attr Attribute) (pat TightPattern)) (return (annotated-pattern attr pat)))))

(def RecordPattern
  (.begin (match-token-value? #\{)
          (.let* ((entries RecordPatEntries)  (_(match-token-value? #\})))
            (return (pattern-record entries)))))

(def RecordPatEntries
    (sepby1 (.let* ((id Identifier) (_(match-token-value? #\:)) (pat TightPattern))
              (cons id pat))
            (match-token-value? #\,)))

(def TightPattern
  (.or RecordPattern BlockPattern BracketedPattern BasePattern))

(def TypePattern
  (.let* ((pat TightPattern) (_(match-token-value? #\:))  (typ Type)) (return (type-pattern pat typ))))

(def AppCtorPattern
  (.let* ((id Identifier)
          (_ (match-token-value? #\( ))
          (empty? (.or (match-token-value? #\) ) #f))
          (args (if empty? [] (sepby1 Pattern (match-token-value? #\,))))
          (_(if (equal? empty? #f)  (match-token-value? #\) ) )))
    (return (pattern-app-ctor id args))))

(def PatternVariant
  (.or AnnotatedPattern TypePattern AppCtorPattern TightPattern))

(def Pattern
  (.let* (pats (sepby1 PatternVariant (match-token-value? #\|)))
    (return (if (length=n? pats 1) (car pats) (pattern-or pats)))))


(defstruct (block-expression expression) (body) transparent: #t)
(def BlockExpression
  (.begin (match-token-value? #\{)
    (.let* ((body  Body) (_(match-token-value? #\})))
      (return (block-expression body)))))

(defstruct (if-expression expression) (exp body-then body-else) transparent: #t)
(def IfExpression
  (.begin (match-token-value? "if") (match-token-value? #\()
    (.let* ((expr ConditionalExpression)
          (_ (match-token-value? #\)))
          (body-then (bracket (match-token-value? #\{) Body (match-token-value? #\})))
          (e (.or (match-token-value? "else") #f))
          (body-else (if (equal? e #f)  [] (bracket (match-token-value? #\{) Body (match-token-value? #\})))))
      (return (if-expression expr body-then body-else)))))

(defstruct (switch expression) (expression cases) transparent: #t)
(def SwitchExpression
    (.begin (match-token-value? "switch") (match-token-value? #\()
      (.let* ((expr ArithmeticExpression)
              (_ (match-token-value? #\) ))
              (_ (match-token-value? #\{ ))
              (empty? (.or (match-token-value? #\} ) #f))
              (cases (if empty? [] Cases))
              (_ (if (equal? empty? #f)  (match-token-value? #\} ) #f)))
        (return (switch expr cases)))))


(defstruct case (pat body) transparent: #t)
(def Case
    (.let* ((pat Pattern)
           (_(match-token-value? "=>"))
           (body Body))
      (return (case pat body))))

(def Cases
  (.begin (.or (match-token-value? #\|) #t) (sepby1 Case (match-token-value? #\|))))


(def Expression
  (.begin (.or AnnotatedExpression IfExpression SwitchExpression
    (.begin (peek (match-token-value? (.or "deposit!" "withdraw!" "assert!" "require!")))
      (.let* (t (item))
        (cond
          ((string=? (get-token-value t) "withdraw!") WithdrawExpression)
          ((string=? (get-token-value t) "deposit!") DepositExpression)
          ((string=? (get-token-value t) "assert!") AssertExpression)
          ((string=? (get-token-value t) "require!") RequireExpression))))
    ArithmeticExpression)))

(defstruct (publish-statement statement) (id expr) transparent: #t)
(def PublishStatement
    (.let* ( (p-id Identifier) (_(match-token-value? "->")) (x-ids (sepby1 Identifier (match-token-value? #\,))) )
      (return (publish-statement p-id x-ids))))

(defstruct (verify-statement statement) (id) transparent: #t)
(def VerifyStatement
    (.let* (ids (sepby1 Identifier (match-token-value? #\,)))
      (return (verify-statement ids))))

(def Typarams
    (.begin #t (sepby1 Identifier (match-token-value? #\,))))

(defstruct (type-declaration statement) (identifier typarams typ) transparent: #t)
(def TypeDeclaration
      (.let*  ((name Identifier)
              (typarams (.or (bracket  (match-token-value? #\() Typarams (match-token-value? #\))) #f))
              (_(match-token-value? #\=))  (typ Type) )
            (return (type-declaration name typarams typ))))


(defstruct (dataAssignmentStatement statement) (identifier typarams variants) transparent: #t)
(def DataAssignmentStatement
    (.let* ((name Identifier)
          (typarams  (.or (bracket (match-token-value? #\() Typarams (match-token-value? #\))) #f))
          (_(match-token-value? #\=)) (variants Variants))
        (return (dataAssignmentStatement name typarams variants))))

(def SubStatement
  (.begin (peek (match-token-value? (.or "verify!" "publish!" "data" "type")))
    (.let* (t (item))
      (cond
        ((string=? (get-token-value t) "verify!") VerifyStatement)
        ((string=? (get-token-value t) "publish!") PublishStatement)
        ((string=? (get-token-value t) "type") TypeDeclaration)
        ((string=? (get-token-value t) "data") DataAssignmentStatement)))))

(defstruct (expression-statement statement) (expr) transparent: #t)
(def ExpressionStatement (.let* ((exp Expression) )
	(return (expression-statement exp))))

(defstruct (assignment-statement statement) (identifier type expr) transparent: #t)
(def AssignmentStatement
    (.let*  (
        (name Identifier)
        (typ (.or (.begin (match-token-value? #\:) Type) #f))
        (_(match-token-value? #\=))
        (expr Expression))
      (return (assignment-statement name typ expr))))


(defstruct (function-declaration statement) (identifier params type expr) transparent: #t)
(def FunctionDeclaration
    (.let* ((name Identifier)
            (_(match-token-value? #\=))
            (params  Params)
            (typ (.or (.begin (match-token-value? #\:) Type) #f))
            (_(match-token-value? "=>"))
            (expr Expression))
          (return (function-declaration name  params typ expr))))

(def LetStatement (.begin (match-token-value? "let") (.or FunctionDeclaration  AssignmentStatement)))

(def Params
  (.let* ((_(match-token-value? #\())
          (params (sepby Param (match-token-value? #\,)))
          (_(match-token-value? #\)))) params))

(defstruct param-data (id typ) transparent: #t)

;; Param ::= Identifier | Identifier:Type
(def Param
  (.let* ((id (.or Identifier FAIL)))
    (.let* ((typ? (.or (.begin (match-token-value? #\:) Type) #f)))
      (return (param-data id typ?))
        )))

(defstruct (annotationStatement statement) (attribute stat) transparent: #t)
(def AnnotationStatement
  (.begin (match-token-value? #\@)
    (.let* ((attr Attribute) (stat  Statement))
        (return (annotationStatement attr stat)))))

(def Statement
  (.or AnnotationStatement LetStatement SubStatement ExpressionStatement))

(def StatementSemicolon (.let* ((stat Statement) (_(match-token-value? #\;))) stat))
(def StatementList (many1 StatementSemicolon))
(def FunctionStatementList (.or StatementList #f))

(defstruct (body expression) (statements expr) transparent: #t)
(def Body
    (.let* ((statements (many StatementSemicolon))
            (expression? (.or Expression #f)))
        (return (body statements expression?))))

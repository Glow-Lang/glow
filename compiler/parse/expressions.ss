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
       (.let* (t (.begin (peek (.or (match-token-type? 'DoubleQuoteStringLiteral)
                                    (match-token-type? 'SingleQuoteStringLiteral)))
                         (item)))
         (return (string-literal (get-token-value t))))
       (.let* (t (.begin (peek (match-token-type? 'IntegerLiteral)) (item)))
         (return (numeric-literal (get-token-value t))))))

(defstruct (bracket-expression expression) (exps) transparent: #t)
(def BracketExpression
  (.begin (equal-token-value? #\()
    (.let* (
           (exps (sepby Expression  (equal-token-value? #\,)))
            (_(equal-token-value? #\) )))
          (return (if (length=n? exps 1) (car exps) (bracket-expression exps))))))

(def TightExpression
  (.begin #t (.or BracketExpression CompoundExpression RecordExpr BlockExpression Identifier Literal)))

(def PrimaryExpression
  ; CallExpression | DotExpression | TightExpression
  (.let* ((a TightExpression)
          (bs (many ArgumentsOrDot)))
    (for/fold (a a) ((b bs))
      (cond ((arguments? b) (call-expression a b))
            (else (dot-expression a b))))))

(defstruct (arguments expression) (list) transparent: #t)
(def Arguments
  (.let* ((_ (equal-token-value? #\())
          (empty? (.or (equal-token-value? #\)) #f))
          (args (if empty? [] ArgumentList))
          (_(if (equal? empty? #f)  (equal-token-value? #\)) )))
    (return (arguments args))))

(def ArgumentList (.begin #t (sepby1 Expression (equal-token-value? #\,))))

(def ArgumentsOrDot
  (.or Arguments (.begin (equal-token-value? #\.) Identifier)))

(defstruct (unary-expression expression) (op expression) transparent: #t)
(def UnaryExpression
    (.let* ((op (.or (member-token-value? ["+" "-" "~~~" "!"]) #f))
            (exp TypedOrNonTypedPrimaryExpression))
              (if op  (return (unary-expression op exp)) exp)))

(defstruct (exponentiation-expression operator) () transparent: #t)
(def ExponentiationExpression
  (Operator exponentiation-expression UnaryExpression "**"))

(defstruct (multiplicative-expression operator) () transparent: #t)
(def MultiplicativeExpression
  (.let* ((g (Operator multiplicative-expression ExponentiationExpression MultiplicativeOperator))
           (e (.or (member-token-value? ["*" "/" "%"])  #f))
           (right (if e (.let* (a MultiplicativeExpression)(multiplicative-expression g e a)) g))) right))


(def MultiplicativeOperator (.or "*" "/" "%"))

(defstruct (additive-expression operator) () transparent: #t)
(def AdditiveExpression
  (.let* ((g (Operator additive-expression MultiplicativeExpression (.or "+" "-")))
           (e (.or (equal-token-value? "+") (equal-token-value? "-") #f))
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
               (_(equal-token-value? #\( ) )
               (arg-expr Expression) ; TODO: multiple Arguments
               (_(equal-token-value? #\) )) )
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
    (.let* ((id Identifier) (_(equal-token-value? "->")) (exp Expression))
          (return (deposit-expression id exp))))

(defstruct (withdraw-expression expression) (id exp) transparent: #t)
(def WithdrawExpression
    (.let* ((id Identifier)
            (_(equal-token-value? "<-"))
            (exp ArithmeticExpression))
          (return (withdraw-expression id exp))))

(defstruct (annotated-expression expression) (attr  expr) transparent: #t)
(def AnnotatedExpression
  (.begin (equal-token-value? #\@)
    (.let* ((attr Attribute) (exp PrimaryExpression))
      (return (annotated-expression attr exp)))))

(defstruct (dot-expression expression) (expr id) transparent: #t)

(defstruct (compound-expression expression) (expressions) transparent: #t)
(def CompoundExpression
  (.begin (equal-token-value? #\[)
    (.let*  ((ce (sepby1  Expression (equal-token-value? #\,)))
              (_(equal-token-value? #\])))
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
      (.begin (equal-token-value? #\')
        (.let* ( (id IdentifierTokenName)) (return (type-var id))))))

(def AnnotatedType
   (.begin (equal-token-value? #\@)
        (.let* ((attr Attribute) (typ BaseType)) (return (annotated-type attr typ)))))

(def BracketedType
  (.begin (equal-token-value? #\( )
        (.let* ((typs (sepby1 BaseType  (equal-token-value? #\,))) (_(equal-token-value? #\))  ))
                 (return (if (length=n? typs 1) (car typs) (type-tuple typs))))))
(def RecordType
      (.begin (equal-token-value? #\{)
        (.let* ((entries  RecordTypeEntries)  (_(equal-token-value? #\}))) (return (type-record entries)))))

(def RecordTypeEntry
    (.let* ((id Identifier) (_(equal-token-value? #\:)) (typ Type)) (cons id typ)))

(def RecordTypeEntries
    (.let* (entries (sepby1 RecordTypeEntry (equal-token-value? #\,)))
        (return entries)))

(defstruct (type-expression expression) (expr typ) transparent: #t)
(def TypedOrNonTypedPrimaryExpression
  (.let* ((exp PrimaryExpression) (typ? (.or (.begin (equal-token-value? #\:)  BaseType) #f)))
      (if (equal? typ? #f) exp  (return (type-expression exp typ?)))))

(def Type
  (.or RecordType BracketedType  AnnotatedType BaseType))

(def Variant
    (.let* ((name Identifier)
            (marker? (.or (equal-token-value? #\( ) #f)))
        (if (equal? marker? #f)
            name
            (.let* ((typs (sepby1 Type  (equal-token-value? #\,)))
                    (_(equal-token-value? #\) )) )
              (cons name typs)))))

(def Variants
  (.begin (.or (equal-token-value? #\|) #t)
          (sepby1 Variant (equal-token-value? #\|))))

(defstruct (record-expr expression) (entries) transparent: #t)
(def RecordExpr
  (.begin (equal-token-value? #\{)
          (.let* ((records RecordExprEntries)
                  (_(equal-token-value? #\})))
            (return (record-expr records)))))

(def RecordExprEntries
  (sepby1 (.let* ((id Identifier)
                  (_(equal-token-value? #\:))
                  (expr ConditionalExpression))
            (return (cons id expr)) )
          (equal-token-value? #\,)))

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
        (.let* (_(equal-token-value? #\_)) (return (pattern-blank)))
        (.let* (lit Literal) (return (pattern-lit lit)))))

(def BracketedPattern
  (.begin  (equal-token-value? #\()
           (.let* ((rst (sepby1 Pattern (equal-token-value? #\,))) (_(equal-token-value? #\))))
                (return (if (length=n? rst 1) (car rst) (pattern-tuple rst))))))

(def BlockPattern
  (.begin (equal-token-value? #\[)
          (.let* ((rst (sepby1 Pattern (equal-token-value? #\,))) (_(equal-token-value? #\])))
                (return (pattern-list rst)))))

(def AnnotatedPattern
  (.begin (equal-token-value? #\@)
          (.let* ((attr Attribute) (pat TightPattern)) (return (annotated-pattern attr pat)))))

(def RecordPattern
  (.begin (equal-token-value? #\{)
          (.let* ((entries RecordPatEntries)  (_(equal-token-value? #\})))
            (return (pattern-record entries)))))

(def RecordPatEntries
    (sepby1 (.let* ((id Identifier) (_(equal-token-value? #\:)) (pat TightPattern))
              (cons id pat))
            (equal-token-value? #\,)))

(def TightPattern
  (.or RecordPattern BlockPattern BracketedPattern BasePattern))

(def TypePattern
  (.let* ((pat TightPattern) (_(equal-token-value? #\:))  (typ Type)) (return (type-pattern pat typ))))

(def AppCtorPattern
  (.let* ((id Identifier)
          (_ (equal-token-value? #\( ))
          (empty? (.or (equal-token-value? #\) ) #f))
          (args (if empty? [] (sepby1 Pattern (equal-token-value? #\,))))
          (_(if (equal? empty? #f)  (equal-token-value? #\) ) )))
    (return (pattern-app-ctor id args))))

(def PatternVariant
  (.or AnnotatedPattern TypePattern AppCtorPattern TightPattern))

(def Pattern
  (.let* (pats (sepby1 PatternVariant (equal-token-value? #\|)))
    (return (if (length=n? pats 1) (car pats) (pattern-or pats)))))


(defstruct (block-expression expression) (body) transparent: #t)
(def BlockExpression
  (.begin (equal-token-value? #\{)
    (.let* ((body  Body) (_(equal-token-value? #\})))
      (return (block-expression body)))))

(defstruct (if-expression expression) (exp body-then body-else) transparent: #t)
(def IfExpression
  (.begin (equal-token-value? "if") (equal-token-value? #\()
    (.let* ((expr ConditionalExpression)
          (_ (equal-token-value? #\)))
          (body-then (bracket (equal-token-value? #\{) Body (equal-token-value? #\})))
          (e (.or (equal-token-value? "else") #f))
          (body-else (if (equal? e #f)  [] (bracket (equal-token-value? #\{) Body (equal-token-value? #\})))))
      (return (if-expression expr body-then body-else)))))

(defstruct (switch expression) (expression cases) transparent: #t)
(def SwitchExpression
    (.begin (equal-token-value? "switch") (equal-token-value? #\()
      (.let* ((expr ArithmeticExpression)
              (_ (equal-token-value? #\) ))
              (_ (equal-token-value? #\{ ))
              (empty? (.or (equal-token-value? #\} ) #f))
              (cases (if empty? [] Cases))
              (_ (if (equal? empty? #f)  (equal-token-value? #\} ) #f)))
        (return (switch expr cases)))))


(defstruct case (pat body) transparent: #t)
(def Case
    (.let* ((pat Pattern)
           (_(equal-token-value? "=>"))
           (body Body))
      (return (case pat body))))

(def Cases
  (.begin (.or (equal-token-value? #\|) #t) (sepby1 Case (equal-token-value? #\|))))


(def Expression
  (.begin (.or AnnotatedExpression IfExpression SwitchExpression
    (.begin (peek (member-token-value? ["deposit!" "withdraw!" "assert!" "require!"]))
      (.let* (t (item))
        (cond
          ((string=? (get-token-value t) "withdraw!") WithdrawExpression)
          ((string=? (get-token-value t) "deposit!") DepositExpression)
          ((string=? (get-token-value t) "assert!") AssertExpression)
          ((string=? (get-token-value t) "require!") RequireExpression))))
    ArithmeticExpression)))

(defstruct (publish-statement statement) (id expr) transparent: #t)
(def PublishStatement
    (.let* ( (p-id Identifier) (_(equal-token-value? "->")) (x-ids (sepby1 Identifier (equal-token-value? #\,))) )
      (return (publish-statement p-id x-ids))))

(defstruct (verify-statement statement) (id) transparent: #t)
(def VerifyStatement
    (.let* (ids (sepby1 Identifier (equal-token-value? #\,)))
      (return (verify-statement ids))))

(def Typarams
    (.begin #t (sepby1 Identifier (equal-token-value? #\,))))

(defstruct (type-declaration statement) (identifier typarams typ) transparent: #t)
(def TypeDeclaration
      (.let*  ((name Identifier)
              (typarams (.or (bracket  (equal-token-value? #\() Typarams (equal-token-value? #\))) #f))
              (_(equal-token-value? "="))  (typ Type) )
            (return (type-declaration name typarams typ))))


(defstruct (dataAssignmentStatement statement) (identifier typarams variants) transparent: #t)
(def DataAssignmentStatement
    (.let* ((name Identifier)
          (typarams  (.or (bracket (equal-token-value? #\() Typarams (equal-token-value? #\))) #f))
          (_(equal-token-value? "=")) (variants Variants))
        (return (dataAssignmentStatement name typarams variants))))

(def SubStatement
  (.begin (peek (member-token-value? ["verify!" "publish!" "data" "type"]))
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
        (typ (.or (.begin (equal-token-value? #\:) Type) #f))
        (_(equal-token-value? "="))
        (expr Expression))
      (return (assignment-statement name typ expr))))


(defstruct (function-declaration statement) (identifier params type expr) transparent: #t)
(def FunctionDeclaration
    (.let* ((name Identifier)
            (_(equal-token-value? "="))
            (params  Params)
            (typ (.or (.begin (equal-token-value? #\:) Type) #f))
            (_(equal-token-value? "=>"))
            (expr Expression))
          (return (function-declaration name  params typ expr))))

(def LetStatement (.begin (equal-token-value? "let") (.or FunctionDeclaration  AssignmentStatement)))

(def Params
  (.let* ((_(equal-token-value? #\())
          (params (sepby Param (equal-token-value? #\,)))
          (_(equal-token-value? #\)))) params))

(defstruct param-data (id typ) transparent: #t)

;; Param ::= Identifier | Identifier:Type
(def Param
  (.let* ((id (.or Identifier FAIL)))
    (.let* ((typ? (.or (.begin (equal-token-value? #\:) Type) #f)))
      (return (param-data id typ?))
        )))

(defstruct (annotationStatement statement) (attribute stat) transparent: #t)
(def AnnotationStatement
  (.begin (equal-token-value? #\@)
    (.let* ((attr Attribute) (stat  Statement))
        (return (annotationStatement attr stat)))))

(def Statement
  (.or AnnotationStatement LetStatement SubStatement ExpressionStatement))

(def StatementSemicolon (.let* ((stat Statement) (_(equal-token-value? #\;))) stat))
(def StatementList (many1 StatementSemicolon))
(def FunctionStatementList (.or StatementList #f))

(defstruct (body expression) (statements expr) transparent: #t)
(def Body
    (.let* ((statements (many StatementSemicolon))
            (expression? (.or Expression #f)))
        (return (body statements expression?))))

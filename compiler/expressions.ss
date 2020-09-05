(import :drewc/smug "./lexical" ) 
;;(import :drewc/smug :mukn/glow/compiler/lexical)
(export #t)

(defstruct expression ())
(defstruct statement ())

(defstruct (identifiers expression) ())

(def (parse-operator side op)
  (.or (.let* ((lhs side) (pop (match-token-value? op))
               (rhs side))
         (return (values lhs pop rhs)))
       (.let* (s side) (return (values s #f #f)))))

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
(defstruct (null-literal literal) () transparent: #t)
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

(defstruct (bracket-expression expression) (exp) transparent: #t)
(def BracketExpression 
  (.begin (match-token-value? #\()
    (.let* (
           (exp ArithmeticExpression )
            (_(match-token-value? #\) )))
          (return (bracket-expression exp)))))

(def PrimaryExpression 
  (.begin #t (.or CallExpression  BracketExpression Identifier Literal)))


(defstruct (arguments expression) (list) transparent: #t) 
(def Arguments
  (.let* ((_ (match-token-value? #\())
          (empty? (.or (match-token-value? #\)) #f))
          (args (if empty? [] ArgumentList))
          (_(if (equal? empty? #f)  (match-token-value? #\)) )))
    (return (arguments args))))

(def ArgumentList (.begin #t (sepby1 PrimaryExpression (match-token-value? #\,))))


(defstruct (unary-expression expression) (op expression) transparent: #t)
(def UnaryExpression
  (.begin
    #t
    (.or (.let* ((op (match-token-value? (.or "+" "-" "~" "!")))
                 (exp PrimaryExpression))
           (return (unary-expression op exp))) PrimaryExpression)))

(defstruct (exponentiation-expression operator) () transparent: #t)
(def ExponentiationExpression
  (Operator exponentiation-expression UnaryExpression "**"))


(defstruct      (multiplicative-expression operator) () transparent: #t)
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
  (Operator shift-expression AdditiveExpression (.or  ">>>" "<<" ">>")))


(defstruct (relational-expression operator) () transparent: #t)
(def RelationalExpression
  (Operator relational-expression ShiftExpression
            (.or "<=" ">=" "<" ">")))


(defstruct (equality-expression operator) () transparent: #t)
(def EqualityExpression
  (.or (Operator equality-expression RelationalExpression "==")
       (Operator equality-expression RelationalExpression "!=")
       (Operator equality-expression RelationalExpression "===")
       (Operator equality-expression RelationalExpression "!==")))


(defstruct (bitwise-and-expression operator) () transparent: #t)
(def BitwiseANDExpression
  (Operator bitwise-and-expression EqualityExpression "&"))

(defstruct (bitwise-xor-expression operator) () transparent: #t)
(def BitwiseXORExpression
  (Operator bitwise-xor-expression BitwiseANDExpression "^"))

(defstruct (bitwise-or-expression operator) () transparent: #t)
(def BitwiseORExpression
  (Operator bitwise-or-expression BitwiseXORExpression "|"))

(defstruct (logical-and-expression operator) () transparent: #t)
(def LogicalANDExpression
  (Operator logical-and-expression BitwiseORExpression "&&"))

(defstruct (logical-or-expression operator) () transparent: #t)
(def LogicalORExpression
  (Operator logical-or-expression LogicalANDExpression "||"))

(def ConditionalExpression (.begin #t LogicalORExpression))
(def ArithmeticExpression (.begin #t ShiftExpression))


(def Attribute
  (.or Identifier Arguments))


(defstruct (call-expression expression) (id arguments)  transparent: #t)
(def CallExpression
    (.let* ((id Identifier)
            (arguments Arguments))
          (return (call-expression  id arguments))))


(defstruct (require-expression expression) (exp) transparent: #t)
(def RequireExpression 
  (.begin (match-token-value? "require") (match-token-value? "!")
    (.let* ((exp ConditionalExpression))
          (return (require-expression exp)))))

(defstruct (assert-expression expression) (exp) transparent: #t)
(def AssertExpression 
  (.begin (match-token-value? "assert") (match-token-value? "!")
    (.let* ((exp ConditionalExpression))
          (return (assert-expression exp)))))

(defstruct (deposit-expression expression) (exp) transparent: #t)
(def DepositExpression 
  (.begin (match-token-value? "deposit") (match-token-value? "!")
    (.let* (exp ArithmeticExpression)
          (return (deposit-expression exp)))))

(defstruct (withdraw-expression expression) (id exp) transparent: #t)
(def WithdrawExpression 
  (.begin (match-token-value? "withdraw") (match-token-value? "!")
    (.let* ((id Identifier)
            (exp ArithmeticExpression))
          (return (withdraw-expression id exp)))))

(defstruct (annotated-expression expression) (attr  expr) transparent: #t)
(def AnnotatedExpression 
  (.begin (match-token-value? #\@) 
    (.let* ((attr Attribute) (exp PrimaryExpression))
      (return (annotated-expression attr exp)))))

(defstruct (dot-expression expression) (expr id) transparent: #t)
(def DotExpression 
  (.let* ((exp (.or CallExpression PrimaryExpression)) (_(match-token-value? #\.)) (id Identifier))
      (return (dot-expression exp id))))

(defstruct (compound-expression expression) (expressions) transparent: #t)
(def CompoundExpression 
  (.begin (match-token-value? #\[) 
    (.let*  ((ce (sepby1  ArithmeticExpression (match-token-value? #\,)))  
              (_(match-token-value? #\])))
      (return (compound-expression ce)))))

(def IdentifierTokenName 
  (.begin (peek (match-token-type? 'IdentifierName))
          (peek (match-token-value? (.not ReservedWord)))
          (.let* (t (item)) (get-token-value t))))

(defstruct type (attr kind) transparent: #t)
(def BaseType 
    (.or
      (.let* (id IdentifierTokenName) (return (type #f id)))
      (.begin (match-token-value? #\')
        (.let* ( (id IdentifierTokenName)) (return (type #t id))))))

(def AnnotatedType 
   (.begin (match-token-value? #\@) 
        (.let* ((attr Attribute) (typ BaseType)) (return (type attr typ)))))

(def BracketedType
  (.begin (match-token-value? #\( )
        (.let* ((typ (sepby1 BaseType  (match-token-value? #\,))) (_(match-token-value? #\))  ))
                 (return (type #f typ)))))
(def RecordedType
      (.begin (match-token-value? #\{)
        (.let* ((records  RecordTypeEntries)  (_(match-token-value? #\}))) (return (type #f records)))))

(defstruct record-type (id typ) transparent: #t)
(def RecordType 
    (.let* ((id Identifier) (_(match-token-value? #\:)) (typ Type)) (record-type id typ)))

(def RecordTypeEntries
    (.let* (types (sepby1 RecordType (match-token-value? #\,)))
        (return types)))

(defstruct (type-expression expression) (expr typ) transparent: #t)
(def TypeExpression 
  (.let* ((exp PrimaryExpression) (_(match-token-value? #\:)) (typ BaseType))
      (return (type-expression exp typ))))

(def Type
  (.or RecordedType BracketedType  AnnotatedType BaseType))

(def Variant
  (.or 
    (.let* ((name Identifier) (_(match-token-value? #\())
      (typs (sepby1 Type  (match-token-value? #\,)))
      (_(match-token-value? #\))) )
    typs) IdentifierTokenName))

(def Variants (.begin #t (sepby1 Variant (match-token-value? #\|))))


(defstruct (record-expr-entries expression) (entries) transparent: #t)
(defstruct (record-expr-entry expression) (id exp) transparent: #t)
(def RecordExprEntries
    (.let* ((entries (sepby1 (.let* ((id Identifier) 
              (_(match-token-value? #\:)) (expr ConditionalExpression)) 
                  (return (record-expr-entry id exp)) ) (match-token-value? #\,))))
            (record-expr-entries entries)))

(defstruct pattern (pat attri)  transparent: #t)
(def BasePattern 
    (.or  
        (.let* (id Identifier) (return (pattern #f id))) 
        (.let* (_(match-token-value? #\_)) (return (pattern #f #\_)))    
        (.let* (lit Literal) (return (pattern #f lit)))))

(def BracketedPattern
  (.begin  (match-token-value? #\() 
            (.let* ((rst (sepby1 BasePattern (match-token-value? #\,))) (_(match-token-value? #\))))
                (return (pattern #f rst)))))

(def BarPattern 
  (.begin (match-token-value? #\|)
    (.let* (pats (sepby1 BasePattern (match-token-value? #\|))) (return (pattern #f pats)))))

(def BlockPattern
  (.begin  (match-token-value? #\[) 
            (.let* ((rst (sepby1 BasePattern (match-token-value? #\,))) (_(match-token-value? #\])))
                (return (pattern #f rst)))))

(def AnnotatedPattern
  (.begin (match-token-value? #\@)
            (.let* ((attr Attribute) (pat BasePattern)) (return (pattern attr pat)))))

(def TypedPattern
  (.let* ((pat BasePattern) (_(match-token-value? #\:))  (typ Type)) (return (pattern typ pat))))

(def RecordedPattern
  (.begin (match-token-value? #\{)
            (.let* ((records RecordPatEntries)  (_(match-token-value? #\}))) 
                (return (pattern #f records))))) 

(defstruct record-pat (id pattern) transparent: #t)
(def RecordPatEntries
    (.let* (patterns (sepby1 (.let* ((id Identifier) (_(match-token-value? #\:)) (pat BasePattern)) 
                (record-pat id pat) ) (match-token-value? #\,)))
            (return patterns)))

(def Pattern (.or RecordedPattern AnnotatedPattern BlockPattern BarPattern BracketedPattern TypedPattern BasePattern))


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
      
(defstruct (switch expression) (expressions cases) transparent: #t)
(def SwitchExpression
    (.begin (match-token-value? "switch") (match-token-value? #\()
      (.let* ((expr ArithmeticExpression)
          (_ (match-token-value? #\)))
          (cases (sepby1 (.begin (get-token-value #\|) Case) (match-token-value? #\|))))
      (return (switch expr cases)))))


(defstruct case (pat body) transparent: #t)
(def Case 
    (.let* ((pat Pattern)
           (_(match-token-value? "=>"))
           (body Body))
      (return (case pat body))))


(def Expression 
    (.or AnnotatedExpression TypeExpression DotExpression CompoundExpression
        RecordExprEntries BlockExpression IfExpression SwitchExpression 
        CallExpression
              RequireExpression  AssertExpression DepositExpression WithdrawExpression ArithmeticExpression))




;

(defstruct (publish-statement statement) (ids) transparent: #t)
(def PublishStatement 
  (.begin (match-token-value? "publish") (match-token-value? "!")
    (.let* ((ids (sepby1 Identifier (match-token-value? #\,))) (_(match-token-value? #\;)))
        (return (publish-statement ids)))))

(defstruct (verify-statement statement) (ids) transparent: #t)
(def VerifyStatement 
  (.begin (match-token-value? "verify") (match-token-value? "!")
    (.let* ((ids (sepby1 Identifier (match-token-value? #\,)) ) (_(match-token-value? #\;)))
         (return (verify-statement ids)))))

(def Typarams 
    (.begin #t (sepby1 Identifier (match-token-value? #\,))))

(defstruct (type-declaration statement) (identifier typarams typ) transparent: #t)
(def TypeDeclaration 
    (.begin (match-token-value? "type")
      (.let*  ((name Identifier) 
              (typarams (.or (bracket  (match-token-value? #\() Typarams (match-token-value? #\))) #f))
              (_(match-token-value? #\=))  (typ Type)  (_(match-token-value? #\;)))
            (return (type-declaration name typarams typ)))))


(defstruct (dataAssignmentStatement statement) (identifier typarams variants) transparent: #t)
(def DataAssignmentStatement
  (.begin (match-token-value? "data")
    (.let* ((name Identifier) 
          (typarams  (.or (bracket (match-token-value? #\() Typarams (match-token-value? #\))) #f)) 
          (_(match-token-value? #\=)) (variants Variants))
        (return (dataAssignmentStatement name typarams variants)))))



(def SubStatements (.or VerifyStatement  PublishStatement DataAssignmentStatement TypeDeclaration))

(defstruct (expression-statement statement) (expr) transparent: #t)
(def ExpressionStatement (.let* ((exp Expression) (_(match-token-value? #\;))) 
	(return (expression-statement exp))))

(defstruct (assignment-statement statement) (identifier type expr) transparent: #t)
(def AssignmentStatement 
  (.begin (match-token-value? "let")
    (.let*  (
        (name Identifier)
        (typ (.or (.begin (match-token-value? #\:) Type) #f))
        (_(match-token-value? #\=))
        (expr Expression)
        (_(match-token-value? #\;))) 
      (return (assignment-statement name typ expr)))))


(defstruct (function-declaration statement) (identifier params type expr) transparent: #t)
(def FunctionDeclaration 
  (.begin (match-token-value? "let")
    (.let* ((name Identifier)
            (params  Params)
            (typ (.or (.begin (match-token-value? #\:) Type) #f))
            (_(match-token-value? "=>"))
            (expr Expression)
            (_(match-token-value? #\;))) 
          (return (function-declaration name  params typ expr)))))

(def LetStatements (.or FunctionDeclaration  AssignmentStatement))

(def Params 
  (.let* ((_(match-token-value? #\())  
          (params (sepby1 Param (match-token-value? #\,)))
          (_(match-token-value? #\)))) params))

(defstruct param-data (id typ) transparent: #t)
(def Param  
    (.let* (param  (.or 
        (.let* ((id Identifier) (_ (match-token-value? #\:) ) (typ Type))
          (return (param-data id typ))) (.let* (id Identifier) (return (param-data id #f))) #f)) param)) 


(defstruct (annotationStatement statement) (attribute stat) transparent: #t)
(def AnnotationStatement 
  (.begin (match-token-value? #\@)
    (.let* ((attr Attribute) (stat  SubStatements))
        (return (annotationStatement attr stat)))))

(def Statement 
  (.or  
      LetStatements
      SubStatements 
      ExpressionStatement     
      AnnotationStatement
  ))


(def StatementList (many1 Statement))
(def FunctionStatementList (.or StatementList #f))

(defstruct (body statement) (statements expr) transparent: #t)
(def Body 
    (.let* (statements FunctionStatementList) (expression? (.or Expression #f))
        (return (body statements expression?))))

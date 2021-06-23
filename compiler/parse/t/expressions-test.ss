(export #t)

(import
  :std/test
  :clan/assert
  :mukn/glow/compiler/parse/expressions
  :mukn/glow/compiler/parse/lexical
  :mukn/glow/compiler/parse/parse
  :drewc/smug)

(def expressions-test
  (test-suite "test suite for glow/compiler/parse/expressions"
    (test-case "Parsing Param"
      ;; Invalid
      (assert-equal! (run Param (lexify ")")) #f)

      ;; Identifier
      (let* ((param (run Param (lexify "seller")))
             (id (identifier-name (param-id param))))
        (assert-equal! id "seller"))

      ;; Identifier:Type
      (let* ((param (run Param (lexify "seller:Address")))
             (id (identifier-name (param-id param)))
             (type (type-name-id (param-typ param))))
        (assert-equal! id "seller")
        (assert-equal! type "Address"))
      )

    (test-case "Parsing Params"
      ;; No Parameters
      (assert-equal! (run Params (lexify "()")) '())
      )

    (test-case "Parsing No Param"
      (displayln Param)
      (assert-equal! (run Param (lexify "9")) #f))

    (test-case "parameter"
      ;; Identifier:Type
      (let* ((param (run Param (lexify "seller")))
             (id (identifier-name (param-id param))))
        (assert-equal! id "seller")))

    (test-case "parameter with type"
      ;; Identifier:Type
      (let* ((param (run Param (lexify "seller:Address")))
             (id (identifier-name (param-id param)))
             (type (type-name-id (param-typ param))))
        (assert-equal! id "seller")
        (assert-equal! type "Address")))

    (test-case "parameters"
      (let* ((params (run Params (lexify "(seller, seller2)")))
             (param1 (car params))
             (param2 (cadr params))
             (id1 (identifier-name (param-id param1)))
             (id2 (identifier-name (param-id param2))))
        (assert-equal! id1 "seller")
        (assert-equal! id2 "seller2")))

    (test-case "parameters with types"
      (let* ((params (run Params (lexify "(seller:Address, seller2:Address)")))
             (param1 (car params))
             (param2 (cadr params))
             (id1 (identifier-name (param-id param1)))
             (type1 (type-name-id (param-typ param1)))
             (id2 (identifier-name (param-id param2)))
             (type2 (type-name-id (param-typ param2))))
        (assert-equal! id1 "seller")
        (assert-equal! type1 "Address")
        (assert-equal! id2 "seller2")
        (assert-equal! type2 "Address")))

    (test-case "parameters with one type"
      (let* ((params (run Params (lexify "(seller:Address, seller2)")))
             (param1 (car params))
             (param2 (cadr params))
             (id1 (identifier-name (param-id param1)))
             (type1 (type-name-id (param-typ param1)))
             (id2 (identifier-name (param-id param2)))
             (type2 (param-typ param2)))
        (assert-equal! id1 "seller")
        (assert-equal! type1 "Address")
        (assert-equal! id2 "seller2")
        (assert-equal! type2 #f)))

    (test-case "body"
      (let* ((body (run Body (lexify "2;")))
             (expr (body-expression-expr body))
             (st (body-expression-statements body))
             ;(value (st-expr-value st))
             )
        ;(assert-equal! value "2")
        (assert-equal! expr #f)))

    (test-case "Parsing expressions with {} curly-braces"
      (assert-equal! (run Expression (lexify "{}")) (record-expression []))
      (assert-equal! (run Expression (lexify "{ x }"))
                     (body-expression [] (identifier "x")))
      (assert-equal! (run Expression (lexify "{ x: 1 }"))
                     (record-expression [(cons (identifier "x") (numeric-literal "1"))]))
      (assert-equal! (run Expression (lexify "{ x; 1 }"))
                     (body-expression [(expression-statement (identifier "x"))] (numeric-literal "1")))
      ;; TODO: if/when we add record-field shorthand, it would work like this:
      ;(assert-equal! (run Expression (lexify "{ x, y }"))
      ;               (record-expression [(cons (identifier "x") (identifier "x"))
      ;                                   (cons (identifier "y") (identifier "y"))]))
      (assert-equal! (run Expression (lexify "{ x: 1, y: 2 }"))
                     (record-expression [(cons (identifier "x") (numeric-literal "1"))
                                         (cons (identifier "y") (numeric-literal "2"))]))
      )))

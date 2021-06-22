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

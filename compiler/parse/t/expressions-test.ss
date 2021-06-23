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
      (assert-equal! (run Param (lexify "9")) #f)

      ;; Identifier
      (assert-equal! (run Param (lexify "seller"))
                     (param (identifier "seller") #f))

      ;; Identifier:Type
      (assert-equal! (run Param (lexify "seller:Address"))
                     (param (identifier "seller") (type-name "Address"))))

    (test-case "Parsing Params"
      ;; No Parameters
      (assert-equal! (run Params (lexify "()")) '())

      ;; parameters
      (assert-equal! (run Params (lexify "(seller, seller2)"))
                     [(param (identifier "seller") #f) (param (identifier "seller2") #f)])

      ;; parameters with types
      (assert-equal! (run Params (lexify "(seller:Address, seller2:Address)"))
                     [(param (identifier "seller") (type-name "Address"))
                      (param (identifier "seller2") (type-name "Address"))])

      ;; parameters with one type
      (assert-equal! (run Params (lexify "(seller:Address, seller2)"))
                     [(param (identifier "seller") (type-name "Address"))
                      (param (identifier "seller2") #f)]))

    (test-case "body"
      (assert-equal! (run Body (lexify "2;"))
                     (body-expression [(expression-statement (numeric-literal "2"))] #f)))

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

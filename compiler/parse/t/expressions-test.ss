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
     )))

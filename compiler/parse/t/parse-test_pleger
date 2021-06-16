(export #t)

(import
  :std/test
  :clan/assert
  :mukn/glow/compiler/parse/expressions
  :mukn/glow/compiler/parse/lexical
  :mukn/glow/compiler/parse/parse
  :drewc/smug)


(def parse-test_pleger
  (test-suite "testing about testing"
    (test-case "Parsing No Param"
      (displayln Param)
      (assert-equal! (run Param (lexify "9")) #f)
    )

     (test-case "participant"
      ;; Identifier:Type
      (let* ((param (run Param (lexify "seller")))
             (id (identifier-name (param-data-id param))))
        (assert-equal! id "seller"))
      )
     
      (test-case "participant with type"
         ;; Identifier:Type
         (let* ((param (run Param (lexify "seller:Address")))
             (id (identifier-name (param-data-id param)))
             (type (type-name-id (param-data-typ param))))
        (assert-equal! id "seller")
        (assert-equal! type "Address")
        ))

      (test-case "participants"
         (let* ((params (run Params (lexify "(seller, seller2)")))
                (param1 (car params))
                (param2 (cadr params))
                (id1 (identifier-name (param-data-id param1)))
                (id2 (identifier-name (param-data-id param2)))
                )
        (assert-equal! id1 "seller")
        (assert-equal! id2 "seller2"))

      (test-case "participants with types"
         (let* ((params (run Params (lexify "(seller:Address, seller2:Address)")))
                (param1 (car params))
                (param2 (cadr params))
                (id1 (identifier-name (param-data-id param1)))
                (type1 (type-name-id (param-data-typ param1)))
                (id2 (identifier-name (param-data-id param2)))
                (type2 (type-name-id (param-data-typ param2))))
        (assert-equal! id1 "seller")
        (assert-equal! type1 "Address")
        (assert-equal! id2 "seller2")
        (assert-equal! type2 "Address")))

        (test-case "participants with one type"
         (let* ((params (run Params (lexify "(seller:Address, seller2)")))
                (param1 (car params))
                (param2 (cadr params))
                (id1 (identifier-name (param-data-id param1)))
                (type1 (type-name-id (param-data-typ param1)))
                (id2 (identifier-name (param-data-id param2)))
                (type2 (param-data-typ param2)))
        (assert-equal! id1 "seller")
        (assert-equal! type1 "Address")
        (assert-equal! id2 "seller2")
        (assert-equal! type2 #f)))

      (test-case "body"
         (let* ((body (run Body (lexify "2;")))
                (expr (body-expr body))
                (st (body-statements body))
                ;(value (st-expr-value st))
                )
        ;(assert-equal! value "2")
        (assert-equal! expr #f))
      
  ))))

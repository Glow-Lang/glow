(export #t)

(import
  :std/test
  :clan/assert
  :mukn/glow/compiler/parse/parse)

(def parse-test
  (test-suite "test suite for glow/compiler/parse/parse"
    (test-case "parseStr"
      (assert-equal!
        (parseStr "let publishHello = () => {
                   };")
        '(@module (def publishHello (λ ())))
        ))
    (test-case "parseStr"
      (assert-equal!
        (parseStr "let publishHello = (seller:Address,price) => {
                  };")
        '(@module (def publishHello (λ ((seller : Address) price))))
        ))))

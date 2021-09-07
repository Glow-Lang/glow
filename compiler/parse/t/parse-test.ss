(export #t)

(import
  :std/test
  :clan/assert
  :mukn/glow/compiler/parse/parse)

(def parse-test
  (test-suite "test suite for glow/compiler/parse/parse"
    (test-case "parseStr no parameters"
      (assert-equal!
        (parseStr "let publishHello = () => {
                   };")
        '(@module (def publishHello (λ () (@record))))
        ))
    (test-case "parseStr parameters one type"
      (assert-equal!
        (parseStr "let publishHello = (seller:Address,price) => {
                  };")
        '(@module (def publishHello (λ ((seller : Address) price) (@record))))
        ))
    (test-case "parseStr trailing whitespace and comments"
      (assert-equal!
        (parseStr "let a = 1;
                   // some comments followed by newline
                   /* block comment
                   */
                  ")
        '(@module (def a 1))))

    (test-case "parseStr statements"
      (assert-equal!
        (parseStr "let a = 1;
                   let b = 2;")
        '(@module (def a 1)
                  (def b 2))))

    (test-case "parseStr invalid statements"
      (assert-equal!
        (parseStr "let a = 1;
                   (")
        #f)
      (assert-equal!
        (parseStr "let a = 1;
                   // invalid statement:
                   (")
        #f)
      )))

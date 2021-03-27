(export #t)
(import
  :std/sugar :std/test
  :clan/assert
  ./answer-questions)

(def answer-questions-test
  (test-suite "answer-questions"
    (test-case "Buy-sig-role: Buyer"
      (check-answers
        ["Choose your role:"
         "1) Buyer"
         "2) Seller"
         "Enter number "]
        [["Choose your role:" "Buyer"]]
        "1\n"))
    (test-case "Buy-sig-role: Seller"
      (check-answers
        ["Choose your role:"
         "1) Buyer"
         "2) Seller"
         "Enter number "]
        [["Choose your role:" "Seller"]]
        "2\n"))
    ))

(def (check-answers input-lines answers output)
  (def input (string-join input-lines #\newline))
  (assert-equal!
    (with-output-to-string
      (lambda ()
        (with-input-from-string input
          (lambda ()
            (answer-questions answers)))))
    output))

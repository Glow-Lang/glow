(export #t)
(import
  :std/sugar :std/test :std/misc/hash :std/misc/symbol
  :clan/assert
  ./cli-integration)
;; Self tests for the cli-integration module

(def cli-integration-test
  (test-suite "test suite for glow/t/cli-integration"
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
    (test-case "read-environment: buy-sig"
      (check-environment
        ["(participant:withdraw Seller price)"
         "(return (@tuple))"
         "(@label end0)"
         ""
         "closing#payForSignature interaction finished"
         "Final environment:"
         "Buyer => (address<-0x \"0xa71CEb0990dD1f29C2a064c29392Fe66baf05aE1\")"
         "Seller => (address<-0x \"0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD\")"
         "digest => (bytes<-0x \"0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470\")"
         "price => 1"
         "signature => (<-json Signature \"cdae76c4904373d48f52dc0f56e8c808fde873e672170bb428eabb49e33091ee46bd2b6af00725f0a7b578ef1289030a78ac60ca992ea78a6913fea2f82627481b\")"]
         '((Buyer . (address<-0x "0xa71CEb0990dD1f29C2a064c29392Fe66baf05aE1"))
           (Seller . (address<-0x "0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD"))
           (digest . (bytes<-0x "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))
           (price . 1)
           (signature . (<-json Signature "cdae76c4904373d48f52dc0f56e8c808fde873e672170bb428eabb49e33091ee46bd2b6af00725f0a7b578ef1289030a78ac60ca992ea78a6913fea2f82627481b")))))
    (test-case "supply-parameters: closing"
      (check-parameters
        ["Define parameters"
         "Enter digest"
         ">"
         "Enter price"
         ">"]
        [["digest" "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"]
         ["price" "1"]]
        "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470\n1\n"))
    (test-case "initial block"
      (check-initial-block
        "Max initial block [ Current block number is 250 ]"
        "250\n"))
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

(def (check-environment input-lines output)
  (def input (string-join input-lines #\newline))
  (assert-equal!
    (with-input-from-string input
      (lambda ()
        (def table (read-environment))
        (hash->list/sort table symbol<?)))
    output))

(def (check-parameters input-lines params output)
  (def input (string-join input-lines #\newline))
  (assert-equal!
    (with-input-from-string input
      (lambda ()
        (with-output-to-string
          (lambda ()
            (supply-parameters params)))))
    output))

(def (check-initial-block input output)
  (assert-equal!
    (with-output-to-string
      (lambda ()
        (with-input-from-string input
          (lambda ()
            (set-initial-block)))))
    output))

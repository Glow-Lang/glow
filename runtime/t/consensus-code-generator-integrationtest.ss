(export #t)

(import
  :std/test
  :clan/assert
  :clan/poo/object
  :mukn/ethereum/json-rpc
  :mukn/ethereum/test-contracts
  ../../compiler/multipass
  ../consensus-code-generator
  ../glow-path
  ../program)

(def consensus-code-generator-integrationtest
  (test-suite "test suite for glow/runtime/consensus-code-generator"
    (test-case "testing scratch space for example glow files"
      (ensure-ethereum-connection "pet")
      (map
        (cut apply check-scratch-space-example <>)
        [["buy_sig" 'payForSignature 760]
         ["coin_flip" 'coinFlip 1016]
         ["rps_simple" 'rockPaperScissors 1049]]))))

(def (check-scratch-space-example file-name interaction-name expected-value)
  (let*
    ((output (run-passes (string-append file-name ".glow") pass: 'project))
     (program (parse-compiler-output output))
     (code-generator
       (.call ConsensusCodeGenerator .make
              program
              interaction-name
              ;; arbitrary timeout:
              100
              (.o DefaultToken: PET ))))
    (.call ConsensusCodeGenerator .generate code-generator)
    (assert-equal! (.@ code-generator params-end)
                   expected-value)))

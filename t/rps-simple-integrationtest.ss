(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports :std/misc/hash
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports
  :clan/poo/object :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak
  :clan/persist/content-addressing :clan/persist/db
  :clan/versioning
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/simple-apps :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/testing
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/participant-runtime
  ../runtime/reify-contract-parameters)

(register-test-keys)
(def a-address alice)
(def b-address bob)
(def wagerAmount (wei<-ether .01))

(def rps_simple.glow (source-path "dapps/rps_simple.glow"))

(def (make-agreement)
  (def timeout (ethereum-timeout-in-blocks))
  (def initial-timer-start (+ (eth_blockNumber) timeout))
  (.o
    interaction: "rps_simple#rockPaperScissors"
    participants: (.o A: a-address B: b-address)
    parameters: (hash
                  (wagerAmount (json<- Ether wagerAmount)))
    reference: (.o)
    options: (.o blockchain: "Private Ethereum Testnet" ;; the `name` field of an entry in `config/ethereum_networks.json`
                 escrowAmount: (void) ;; manually done for the current coinflip
                 timeoutInBlocks: timeout ; should be the `timeoutInBlocks` field of the same entry in `config/ethereum_networks.json`
                 maxInitialBlock: initial-timer-start)
    glow-version: (software-identifier)
    code-digest: (digest<-file rps_simple.glow)))

(def rps-simple-integrationtest
  (test-suite "integration test for ethereum/rps_simple"
    (delete-agreement-handshake)
    (ensure-ethereum-connection "pet")
    (ensure-db-connection "testdb")
    (DBG "Ensure participants funded")
    (ensure-addresses-prefunded)
    (DBG "DONE")
    (def compiler-output (run-passes rps_simple.glow pass: 'project show?: #f))
    (def program (parse-compiler-output compiler-output))

    (test-case "rps_simple executes"
      (def agreement (make-agreement))

      (displayln "\nEXECUTING A THREAD ...")
      (def a-thread
        (spawn/name/logged "A"
         (lambda ()
          (parameterize ((current-input-port (open-input-string "2\n"))) ; Scissors
            (run:special-file 'A agreement)))))

      (displayln "\nEXECUTING B THREAD ...")
      (def b-thread
        (spawn/name/logged "B"
         (lambda ()
          (parameterize ((current-input-port (open-input-string "1\n"))) ; Paper
            (run:special-file 'B agreement)))))

      (def a-environment (thread-join! a-thread))
      (def b-environment (thread-join! b-thread))

      (for (variable (hash->list/sort a-environment symbol<?))
        (match variable
          ([name . (type . value)]
            (when (cdr (hash-get b-environment name))
              (check-equal? value (cdr (hash-get b-environment name)))))))

      ;; if A chose scissors, B chose paper, then outcome 2 (A_Wins)
      (def outcome (cdr (hash-get a-environment 'outcome)))
      (display-object
        ["outcome extracted from contract logs, 0 (B_Wins), 1 (Draw), 2 (A_Wins):" UInt256 outcome])
      (check-equal? outcome 2))))

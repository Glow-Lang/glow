#|
$ gxi
> (add-load-path (path-normalize "../gerbil-ethereum"))
> (def d (current-directory))
> (import :mukn/ethereum/scripts/run-geth-test-net)
> (current-directory d)
> (import "t/rps-simpl-integrationtest.ss")
|#
(export #t)

(import
  :gerbil/gambit/ports
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports
  :clan/poo/poo :clan/poo/io (only-in :clan/poo/mop display-poo) :clan/crypto/keccak
  :clan/base :clan/decimal :clan/ports :clan/io :clan/path-config :clan/json
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/transaction-integrationtest
  :mukn/ethereum/t/batch-send-integrationtest
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/ethereum-contract
  ../runtime/ethereum-runtime)

(def A-address alice)
(def B-address bob)

(def participants
  (hash
    (A A-address)
    (B B-address)))
(def arguments
  (hash
    (wagerAmount [Ether . one-ether-in-wei])))

(def compiler-output (run-passes (source-path "examples/rps_simpl.sexp") pass: 'project show?: #f))

(def buy-sig-integrationtest
  (test-suite "integration test for ethereum/rps_simpl"
    (test-case "rps_simpl parses"
      (def program (parse-compiler-output compiler-output))

    ;; TODO: run A and B step in separate threads, using posted transactions to progress their state
    (test-case "rps_simpl executes"
      (def contract (make-Contract
        program: program
        participants: participants
        arguments: arguments))

      (displayln "\nEXECUTING A STEP 1 ...")
      (def runtime-A-1 (make-Runtime contract 'A))
      {execute runtime-A-1}

      (def handshake-json (json<-string (string<-json (read-file-json "run/contract-handshake.json"))))
      (def handshake (<-json ContractHandshake handshake-json))

      (displayln "\nEXECUTING B STEP 1 ...")
      (def runtime-B-1 (make-Runtime contract 'B handshake 'cp0))
      {execute runtime-B-1}
      (def commitment (hash-get (@ runtime-B-1 environment) 'commitment))
      (display-poo
        ["commitment extracted from contract logs: " Digest commitment])

      (def tx-receipt-json (json<-string (string<-json (read-file-json "run/tx-receipt.json"))))
      (def tx-receipt (<-json TransactionReceipt tx-receipt-json))

      (displayln "\nEXECUTING A STEP 2 ...")
      (def runtime-A-2 (make-Runtime contract 'A tx-receipt 'cp0))
      {execute runtime-A-2}
      (def handB (hash-get (@ runtime-A-2 environment) 'handB))
      (display-poo
        ["handB extracted from contract logs: " Nat handB])
      ;; TODO: there are more steps to complete RPS
      (void)))))

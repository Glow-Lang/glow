#|
In another terminal on `gerbil-ethereum`:
$ ../gerbil-ethereum/scripts/run-geth-test-net.ss
In the main terminal in `glow`:
$ export GERBIL_APPLICATION_HOME=$PWD
$ gxi
> (add-load-path (path-normalize "../gerbil-ethereum"))
> (import :std/test "t/rps-simpl-integrationtest.ss")
> (run-tests! rps-simpl-integrationtest)
input UInt256: First player, pick your hand: 0 (Rock), 1 (Paper), 2 (Scissors)
0
|#
(export #t)

(import
  :gerbil/gambit/ports :gerbil/gambit/threads
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

(def rps-simpl-integrationtest
  (test-suite "integration test for ethereum/rps_simpl"
    (test-case "rps_simpl parses"
      (def program (parse-compiler-output compiler-output))

    ;; TODO: run A and B step in separate threads, using posted transactions to progress their state
    (test-case "rps_simpl executes"
      (def contract (make-Contract
        program: program
        participants: participants
        arguments: arguments))

      (def environment #f)

      ;; TODO: erase run/contract-handshake.json from filesystem

      (displayln "\nEXECUTING A THREAD ...")
      (spawn-thread (lambda ()
        (parameterize ((current-input-port (open-input-string "2\n"))) ; Scissors
          (def runtime-A (make-Runtime 'A contract))
          {execute runtime-A}
          (displayln "A finished")
          (set! environment (@ runtime-A environment)))))

      (while (not (file-exists? "run/contract-handshake.json"))
        (displayln "waiting for contract handshake ...")
        (thread-sleep! 1))

      (displayln "\nEXECUTING B THREAD ...")
      (spawn-thread (lambda ()
        (parameterize ((current-input-port (open-input-string "1\n"))) ; Paper
          (def runtime-B (make-Runtime 'B contract))
          {execute runtime-B}
          (displayln "B finished"))))

      (while (not environment)
        (displayln "waiting for A to finish ...")
        (thread-sleep! 1))

      ;; if A chose scissors, B chose paper, then outcome 2 (A_Wins)
      (def outcome (hash-get environment 'outcome))
      (display-poo
        ["outcome extracted from contract logs, 0 (B_Wins), 1 (Draw), 2 (A_Wins):" UInt256 outcome])
      ;; TODO: multiple steps to complete RPS
      (void)))))

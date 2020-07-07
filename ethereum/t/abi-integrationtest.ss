(export #t)

(import
  :gerbil/gambit/os
  :std/misc/ports :std/misc/process :std/srfi/1 :std/test :std/text/hex
  :clan/utils/list :clan/utils/path :clan/utils/path-config :clan/poo/poo
  ../types ../ethereum ../signing ../json-rpc ../transaction ../abi ../tx-tracker
  ./signing-test ./transaction-integrationtest)

(def (compile-solidity src dstdir)
  (def srcdir (path-directory src))
  (def srcfile (path-strip-directory src))
  (create-directory* dstdir)
  (run-process/batch ["env" "-C" srcdir "solc" "--optimize" "--bin" "--abi" "-o" dstdir "--overwrite" srcfile]))

(def test-contract-source (source-path "ethereum/t/test_contract.sol"))
(def test-contract-bin (run-path "t/ethereum/HelloWorld.bin"))

(def (modification-time file)
  (let/cc return
    (def info (with-catch (lambda (_) (return #f)) (lambda () (file-info file #t))))
    (time->seconds (file-info-last-modification-time info))))

(def (test-contract-bytes)
  (unless (and (file-exists? test-contract-bin)
               (<= (or (modification-time test-contract-source) +inf.0)
                   (or (modification-time test-contract-bin) -inf.0)))
    (compile-solidity test-contract-source (path-parent test-contract-bin)))
  (hex-decode (read-file-string test-contract-bin)))

(def contract #f)

(def (ensure-contract)
  (unless contract
    (let (receipt (post-transaction (create-contract croesus (test-contract-bytes))))
      (set! contract (.@ receipt contractAddress)))))

(def abi-integrationtest
  (test-suite "integration test for glow/ethereum/abi"
    (test-case "Contract creation failure due to insufficient gas"
      (check-exception (post-transaction (create-contract croesus (test-contract-bytes) gas: 21000))
                       (match <> ((TxFailed _ (? IntrinsicGasTooLow?)) #t) (_ #f)))
      (check-exception (post-transaction (create-contract croesus (test-contract-bytes) gas: 100000))
                       (match <> ((TxFailed _ (? TransactionRejected?)) #t) (_ #f))))
    (test-case "Call contract function hello with no argument"
      (ensure-contract)
      (def pretx (call-function croesus contract
                                (bytes<-ethereum-function-call ["hello"] [])))
      (def receipt (post-transaction pretx))
      (def block-number (.@ receipt blockNumber))
      (def data (eth_call (CallParameter<-PreTransaction pretx) (1- block-number)))
      (check-equal? data (ethabi-encode [String] ["Hello, World!"]))) ;; TODO
    (test-case "call contract function mul42 with one number argument"
      (def pretx (call-function croesus contract
                                (bytes<-ethereum-function-call ["mul42" UInt256] [47])))
      (def receipt (post-transaction pretx))
      (def block-number (.@ receipt blockNumber))
      (def data (eth_call (CallParameter<-PreTransaction pretx) (1- block-number)))
      (check-equal? data (ethabi-encode [UInt256] [1974])))
    (test-case "call contract function greetings with one string argument"
      (def pretx (call-function croesus contract
                                (bytes<-ethereum-function-call ["greetings" String] ["Croesus"])))
      (def receipt (post-transaction pretx))
      (def block-number (.@ receipt blockNumber))
      (def logs (.@ receipt logs))
      (def receipt-log (first-and-only logs))
      (def log-contract-address (.@ receipt-log address))
      (check-equal? log-contract-address contract)
      (def topic-event (first-and-only (.@ receipt-log topics)))
      (check-equal? topic-event (digest<-function-signature ["greetingsEvent" String]))
      ;; the log data is the encoding of the parameter passed to the event
      (def data (.@ receipt-log data))
      (def result (eth_call (CallParameter<-PreTransaction pretx) (1- block-number)))
      (check-equal? data result)
      (check-equal? data (ethabi-encode [String] ["Greetings, Croesus"])))))

;; TODO: add a stateful function, and check the behavior of eth-call wrt block-number
;; TODO: test the parsing of the HellowWorld.abi JSON descriptor

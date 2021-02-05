(export #t)

(import
  :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports
  :clan/poo/object :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak
  :clan/persist/content-addressing :clan/persist/db
  :clan/versioning
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/testing
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/participant-runtime)

(def buy-sig-integrationtest
  (test-suite "integration test for ethereum/buy-sig"
    (test-case "buy sig runs successfully"
      (delete-agreement-handshake)
      (ensure-ethereum-connection "pet")
      (ensure-db-connection (run-path "testdb"))

      (DBG "Ensure participants funded")
      (ensure-addresses-prefunded)

      (def buyer-address alice)
      (def seller-address bob)
      (def document "abcdefghijklmnopqrstuvwxyz012345")
      (def digest (keccak256<-string document))
      (def price one-ether-in-wei)

      (def timeout (ethereum-timeout-in-blocks))
      (def initial-timer-start (+ (eth_blockNumber) timeout))

      (def buy_sig.glow (source-path "examples/buy_sig.glow"))

      (def agreement
        (.o
         glow-version: (software-identifier)
         interaction: "mukn/glow/examples/buy_sig#payForSignature"
         participants: (.o Buyer: buyer-address Seller: seller-address)
         parameters: (hash
                      (digest0 (json<- Digest digest))
                      (price (json<- Ether price)))
         reference: (.o Buyer: "Purchase #42"
                        Seller: "Sale #101")
         options: (.o blockchain: "Private Ethereum Testnet" ;; the `name` field of an entry in `config/ethereum_networks.json`
                      escrowAmount: (void) ;; not meaningful for buy_sig in particular
                      timeoutInBlocks: timeout ; should be the `timeoutInBlocks` field of the same entry in `config/ethereum_networks.json`
                      maxInitialBlock: initial-timer-start)
         code-digest: (digest<-file buy_sig.glow)))

      (def compiler-output (run-passes buy_sig.glow pass: 'project show?: #f))

      (def program (parse-compiler-output compiler-output))
      (def buyer-balance-before (eth_getBalance alice 'latest))
      (def seller-balance-before (eth_getBalance bob 'latest))

      (displayln "\nEXECUTING BUYER THREAD ...")
      (def buyer-thread
        (spawn/name/logged "Buyer"
         (lambda ()
           (def buyer-runtime
             (make-Runtime role: 'Buyer
                           agreement: agreement
                           program: program))
           {execute buyer-runtime}
           (displayln "buyer finished")
           (@ buyer-runtime environment))))

      (displayln "\nEXECUTING SELLER THREAD ...")
      (def seller-thread
        (spawn/name/logged "Seller"
         (lambda ()
           (def seller-runtime
             (make-Runtime role: 'Seller
                           agreement: agreement
                           program: program))
           {execute seller-runtime}
           (displayln "seller finished"))))


      ;; Get the final environment object from the Buyer
      (def environment (thread-join! buyer-thread))

      (def signature (hash-get environment 'signature))
      (def valid? (message-signature-valid? bob signature digest))
      (def buyer-balance-after (eth_getBalance alice 'latest))
      (def seller-balance-after (eth_getBalance bob 'latest))

      (DDT "DApp completed"
           Signature signature
           Bool valid?
           (.@ Ether .string<-) buyer-balance-before
           (.@ Ether .string<-) seller-balance-before
           (.@ Ether .string<-) buyer-balance-after
           (.@ Ether .string<-) seller-balance-after)

      ;; Check that the signature was valid and that the money transfer happened, modulo gas allowance
      (def gas-allowance (wei<-ether .01))
      (assert! valid?)
      (assert! (<= 0 (- buyer-balance-before buyer-balance-after price) gas-allowance))
      (assert! (<= 0 (- (+ seller-balance-before price) seller-balance-after) gas-allowance)))))

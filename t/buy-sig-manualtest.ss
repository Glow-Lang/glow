(export #t)

(import
  :std/sugar :std/test
  :clan/base :clan/files :clan/json :clan/path-config :clan/shell :clan/versioning
  :clan/poo/object :clan/poo/io
  :clan/crypto/keccak
  :clan/persist/content-addressing :clan/persist/db
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/glow/runtime/participant-runtime :mukn/glow/runtime/reify-contract-parameters
  :mukn/ethereum/testing

  ;; TODO: make sure there's a one-stop-shop to all bindings required for the runtime to work.
  :mukn/ethereum/signing :mukn/ethereum/assets
  :mukn/glow/compiler/syntax-context
  :clan/debug :clan/poo/debug
  )

;; TODO: start the off-chain interaction with a one-sided proposal, and
;; use libp2p to do the off-chain communication

(def buy-sig-manualtest
  (test-suite "integration test for ethereum/buy-sig"
    (test-case "buy sig runs successfully"
      (delete-agreement-handshake)
      (ensure-ethereum-connection "pet")
      (ensure-db-connection (run-path "testdb"))

      (def document "abcdefghijklmnopqrstuvwxyz012345")
      (def digest (keccak256<-string document))
      (def price one-ether-in-wei)
      (def block (eth_blockNumber))
      (def agreement
        (.o
         ;; Filled in "manually" by the DApp from user input
         interaction: "mukn/glow/examples/buy_sig#payForSignature"
         participants: (.o Buyer: alice Seller: bob)
         parameters: (hash
                      (digest (json<- Digest digest))
                      (price (json<- Ether price)))

         ;; Filled in automatically by Glow
         glow-version: (software-identifier)
         code-digest: (digest<-file (source-path "examples/buy_sig.glow"))

         ;; Filled in automatically by the DApp infrastructure
         reference: (.o Buyer: "Purchase #42"
                        Seller: "Sale #101")

         ;; Filled in automatically by the DApp from its blockchain configuration
         options: (.o blockchain: "Private Ethereum Testnet"
                      escrowAmount: (void)
                      timeoutInBlocks: (* 10 (ethereum-timeout-in-blocks))
                      maxInitialBlock: (+ block timeoutInBlocks))))

      (def agreement-path (run-path "t/buy-sig-manualtest-agreement.json"))
      (def agreement-string (json-string<- InteractionAgreement agreement))

      (create-directory* (run-path "t"))

      (clobber-file agreement-path (cut write-json-ln (json<- InteractionAgreement agreement) <>))

      (ensure-addresses-prefunded)

      ;; TODO: if on another machine, it must connect to the same ethereum network(!) Give instructions for that!
      (displayln
       "Here is an agreement, also saved to file " agreement-path "\n"
       agreement-string "\n\n"
       "Please start a Seller on another terminal or machine (with agreement copy) with:\n"
       "  ./glow start-interaction --test --database run/testdb-bob Seller " agreement-path "\n"
       "or\n"
       "  ./glow start-interaction --test --database run/testdb-bob Seller " (escape-shell-token agreement-string) "\n"
       "then press ENTER to start the Buyer in this terminal.")

      (read-line)

      (def buyer-balance-before (eth_getBalance alice 'latest))
      (def seller-balance-before (eth_getBalance bob 'latest))

      ;; Run the Buyer program, get the final environment object from it.
      (def environment (run 'Buyer agreement))

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

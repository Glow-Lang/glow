(export #t)

(import
  :std/sugar
  :clan/base :clan/files :clan/json :clan/path-config :clan/shell :clan/versioning
  :clan/poo/poo :clan/poo/io
  :clan/crypto/keccak
  :clan/persist/content-addressing
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/glow/runtime/ethereum-runtime :mukn/glow/runtime/reify-contract-parameters
  :mukn/ethereum/t/signing-test :mukn/ethereum/t/50-batch-send-integrationtest.ss

  ;; TODO: make sure there's a one-stop-shop to all bindings required for the runtime to work.
  :mukn/ethereum/signing :mukn/ethereum/assets
  :mukn/glow/compiler/syntax-context
  )

;; TODO: start the off-chain interaction with a one-sided proposal, and
;; use libp2p to do the off-chain communication

(def string-to-sign "abcdefghijklmnopqrstuvwxyz012345")

(def block (eth_blockNumber))

(def agreement
  (.o
    glow-version: (software-identifier)
    interaction: "mukn/glow/examples/buy_sig#payForSignature"
    participants: (.o Buyer: alice Seller: bob)
    parameters: (hash
                  (digest0 (json<- Digest (keccak256<-string string-to-sign)))
                  (price (json<- Ether one-ether-in-wei)))
    reference: (.o Buyer: "Purchase #42"
                   Seller: "Sale #101")
    options: (.o blockchain: "Private Ethereum Testnet"
                 escrowAmount: (void)
                 timeoutInBlocks: (* 10 (ethereum-timeout-in-blocks))
                 maxInitialBlock: (+ block timeoutInBlocks))
    code-digest: (digest<-file (source-path "examples/buy_sig.glow"))))

(def agreement-path (run-path "t/buy-sig-manualtest-agreement.json"))
(def agreement-string (json-string<- InteractionAgreement agreement))

(clobber-file agreement-path (cut write-json-ln (json<- InteractionAgreement agreement) <>))

(ensure-addresses-prefunded)

;; TODO: if on another machine, it must connect to the same ethereum network(!) Give instructions for that!
(displayln
 "Here is an agreement, also saved to file " agreement-path "\n"
 agreement-string "\n\n"
 "Please start a Seller on another terminal or machine (with agreement copy) with:\n"
 "  ./glow start-interaction --database run/testdb-bob Seller " agreement-path "\n"
 "or\n"
 "  ./glow start-interaction --database run/testdb-bob Seller " (escape-shell-token agreement-string) "\n"
 "then press ENTER to start the Buyer in this terminal.")

(read-line)

(run 'Buyer agreement)

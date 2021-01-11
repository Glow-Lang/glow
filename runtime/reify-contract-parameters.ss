(export run)
#|
TODO: use DDT from :clan/poo/debug
In both terminals only once at the beginning:
$ . ./env.sh
In one terminal only once at the beginning:
$ ../gerbil-ethereum/scripts/run-ethereum-test-net.ss
In both terminals:
$ gxi
> (import :std/sugar
          :clan/path-config :clan/versioning :clan/poo/poo :clan/poo/mop :clan/crypto/keccak :clan/persist/content-addressing :clan/persist/db
          :mukn/ethereum/ethereum :mukn/ethereum/types :mukn/ethereum/assets :mukn/ethereum/signing :mukn/ethereum/network-config :mukn/ethereum/json-rpc :mukn/ethereum/t/signing-test
          :mukn/glow/path-config :mukn/glow/compiler/syntax-context
          :mukn/glow/runtime/reify-contract-parameters)
In the Buyer terminal:
> (import :mukn/ethereum/t/50-batch-send-integrationtest)
> (ensure-addresses-prefunded)
> (def block (eth_blockNumber))
> block
In the Seller terminal:
> (ensure-ethereum-connection "pet")
> (ensure-db-connection (run-path "testdb-bob"))
> (def block <the block from the other terminal>)
In both terminals:
> (def agreement
   (.o
    glow-version: (software-identifier)
    interaction: "mukn/glow/examples/buy_sig#payForSignature"
    participants: (.o Buyer: alice Seller: bob)
    parameters: (hash
                  (digest0 (json<- Digest (keccak256<-string "abcdefghijklmnopqrstuvwxyz012345")))
                  (price (json<- Ether one-ether-in-wei)))
    reference: (.o Buyer: "Purchase #42"
                   Seller: "Sale #101")
    options: (.o blockchain: "Private Ethereum Testnet"
                 escrowAmount: (void)
                 timeoutInBlocks: (ethereum-timeout-in-blocks)
                 maxInitialBlock: (+ block timeoutInBlocks))
    code-digest: (digest<-file (source-path "examples/buy_sig.glow"))))
In the Buyer terminal:
> (run 'Buyer agreement)
In the Seller terminal:
> (run 'Seller agreement)
|#

(import :std/iter
        :std/pregexp
        :std/misc/string
        :std/text/json
        :clan/json
        :clan/poo/poo
        :clan/poo/mop
        :clan/path-config
        :mukn/ethereum/network-config
        :mukn/ethereum/json-rpc
        :mukn/glow/compiler/syntax-context
        :mukn/glow/compiler/multipass
        :mukn/glow/compiler/passes
        ./program
        ./ethereum-contract
        ./ethereum-runtime)

(.def io-context:terminal
  setup:
  (lambda () (assert! (input-port? (current-input-port))))
  teardown:
  (lambda () (void))
  send-handshake:
  (lambda (handshake)
    (displayln "send this handshake to the other participants:")
    (pretty-print-json (json<- AgreementHandshake handshake)))
  receive-handshake:
  (lambda ()
    (def handshake-json (json<-port (current-input-port)))
    (<-json AgreementHandshake handshake-json)))


;; interaction-agreement->contract : InteractionAgreement -> Contract
(def (interaction-agreement->contract a)
  (defvalues (modpath name)
    (match (pregexp-match "^([^#]*)#([^#]*)$" (.@ a interaction))
      ([_ path name] (values path name))))
  (def path (glow-module-path->path modpath))
  (def compiler-output (run-passes path pass: 'project show?: #f))
  (def program (parse-compiler-output compiler-output))
  ; participants
  (def participants (monomorphic-poo->hash-table (.@ a participants)))
  ; agruments
  (def arguments
    (list->hash-table
     (for/collect ((p (hash->list (.@ a parameters))))
       (match p
         ((cons k v)
          ; TODO: change surface names to alpha-converted names
          (def t {lookup-type program k})
          ; TODO: change JSON values into TypeValuePairs
          (cons k (cons t (<-json t v))))))))
  (make-Contract
    program: program
    participants: participants
    arguments: arguments
    initial-timer-start: (.@ a options maxInitialBlock)
    timeout: (.@ a options timeoutInBlocks)))

;; run : Symbol InteractionAgreement -> ???
(def (run role a)
  (def contract (interaction-agreement->contract a))
  ; ???, depends on whether the given role goes first
  ; if the given role goes first, then:
  ;   deploy the contract, output the AgreementHandshake
  ; otherwise:
  ;   wait for the one who does go first
  ;   to give the AgreementHandshake
  ; --------------------------------
  ;; TODO: in runtime add a Poo object IOContext to handle
  ;;       exchanging AgreementHandshake instead of a `contract-handshake.json` file
  (def runtime
    (make-Runtime role: role
                  agreement: a
                  contract: contract
                  current-code-block-label: 'begin0 ;; TODO: grab the start label from the compilation output, instead of 'begin0
                  current-label: 'begin ;; TODO: grab the start label from the compilation output, instead of 'begins
                  io-context: io-context:terminal))
  {execute runtime}
  ())

;; --------------------------------------------------------

;; glow-module-path->path : String -> PathString
(def (glow-module-path->path s)
  (cond
    ((string-prefix? "mukn/glow/" s)
     (let ((s (string-trim-prefix "mukn/glow/" s)))
       (source-path (string-append s ".glow"))))
    (else
     (error 'glow-module-path->path "given:" s))))

;; monomorphic-poo->hash-table : [MonomorphicPooof V] -> [Hashof Symbol V]
(def (monomorphic-poo->hash-table p) (list->hash-table (.alist p)))

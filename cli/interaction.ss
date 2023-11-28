(export #t)

(import
  ;; gerbil
  :gerbil/expander
  :gerbil/gambit
  :std/cli/getopt
  :std/cli/multicall
  :std/cli/print-exit
  :std/format
  :std/generic
  :std/iter
  :std/misc/hash
  :std/misc/path
  :std/misc/repr
  :std/misc/string
  :std/pregexp
  :std/sort
  :std/srfi/1
  :std/srfi/13
  :std/sugar
  :std/text/json
  ;; gerbil-utils
  :clan/base
  :clan/cli
  :clan/config
  :clan/filesystem
  :clan/hash
  :clan/json
  :clan/path-config
  :clan/string
  ;; gerbil-poo
  :clan/poo/brace
  :clan/poo/cli
  :clan/poo/debug
  :clan/poo/object
  :clan/persist/db
  :clan/persist/content-addressing
  :clan/versioning
  :clan/pure/dict/symdict
  ;; gerbil-ethereum
  :clan/ethereum/assets
  :clan/ethereum/cli
  :clan/ethereum/ethereum
  :clan/ethereum/network-config
  :clan/ethereum/types
  :clan/ethereum/json-rpc
  :clan/ethereum/known-addresses
  :clan/ethereum/test-contracts ; TODO: should this be more "dynamic", only imported when the network is known to be pet?
  ;; glow
  :mukn/glow/runtime/participant-runtime
  :mukn/glow/runtime/reify-contract-parameters
  :mukn/glow/runtime/program
  :mukn/glow/runtime/terminal-codes
  :mukn/glow/runtime/glow-path
  (only-in :mukn/glow/compiler/alpha-convert/alpha-convert init-syms)
  :mukn/glow/compiler/passes
  :mukn/glow/compiler/multipass
  :mukn/glow/compiler/syntax-context
  :mukn/glow/cli/contacts
  :mukn/glow/cli/identities
  ./utils)

(def (ask-option name options)
  (def options-count (length options))
  ;; TODO: Move this somewhere else, probably gerbil-utils?
  ;; TODO: Long term goal is for the options spec to be declarative enough,
  ;; such that we can generate the gui or cli frontends.
  ;; This approach requires less code and provides consistent behaviours for the user.
  (def (range a b)
    (def ∆ (- b a))
    (if (<= 0 ∆) (iota ∆ a) (iota (- ∆) a -1)))
  (def indexed-options (map cons (range 0 options-count) options))
  (displayln BOLD name ":" END)
  (for ((indexed-option indexed-options))
    (def index (car indexed-option))
    (display (string-append (number->string (+ index 1)) ") "))
    (match (cdr indexed-option)
      ([option . (type . annotation)]
        (displayln option " - " (.call type .string<- annotation)))
      ([option . option-short]
        (displayln option-short))
      (option
        (displayln option))))
  (display-prompt "Enter number")
  (let* ((input (read-line)))
    (when (equal? input #!eof) (error "EOF"))
    (def response (string->number input))
    (if (and response (<= 1 response options-count))
      (begin
        (displayln)
        (match (list-ref options (- response 1))
          ([option . _]
            option)
          (option
            option)))
      (begin
        (displayln FAIL "Invalid selection\n" END)
        (ask-option name options)))))

(def (ask-number default-number: (default-number #f) prompt)
  (display-prompt prompt)
  (def input (read-line))
  (displayln)
  (if (equal? input "") default-number
      (.call Nat .<-string input)))

;; TODO: Catch parsing errors and show example inputs.
(def (ask-address name)
  (def known-addresses
    (map
      (match <>
        ([nickname . address]
          (cons nickname [Address . address])))
      (hash->list/sort address-by-nickname string<?)))
  (def chosen-nickname (ask-option name known-addresses))
  (hash-get address-by-nickname (string-downcase chosen-nickname)))

(def (ask-asset name network)
  (def known-assets
    (for/collect ((p (hash->list/sort asset-table symbol<?))
                  when (equal? (asset->network (cdr p)) network))
      (cons* (symbol->string (car p)) Asset (cdr p))))
  (def chosen-asset (ask-option name known-assets))
  (hash-get asset-table (string->symbol chosen-asset)))

(def (display-prompt name)
  (displayln CYAN name)
  (display (string-append "> " END))
  (force-output))

(def (ask-application)
  (def apps (get-glow-app-names))
  (ask-option "Choose application" (map cons apps apps)))

(def (ask-interaction interactions)
  (match interactions
    ([]
      (error "No interactions found"))
    ([interaction]
      interaction)
    (else
      (ask-option "Choose interaction" interactions))))

(def (get-or-ask-participants participants selected-identity selected-role role-names contacts)
  (displayln BOLD "Assign roles" END)
  (for ((role-name role-names))
    (if (equal? selected-role role-name)
      (hash-put! participants role-name
        (hash-get address-by-nickname selected-identity))
      (get-or-ask
        participants
        role-name
        (lambda ()
          (ask-address (string-append "Select address for " (symbol->string role-name)))))))
  participants)

;; get-or-ask-assets : (Hashof Symbol Asset) (Listof Symbol) Network -> (Hashof Symbol AssetType)
(def (get-or-ask-assets assets asset-names network)
  (displayln BOLD "Assign assets" END)
  (for ((asset-name asset-names))
    (get-or-ask assets
      asset-name
      (lambda ()
        (ask-asset (string-append "Select asset for " (symbol->string asset-name)) network))))
  assets)

(def (console-input type name tag)
  (display-prompt
    (string-append
      "Enter "
      (if (u8vector? name) (bytes->string name) (symbol->string name))
      (if tag (str tag) "")))
  (try
    (let (input (read-line))
      (.call type .<-string input))
    (catch (e)
      (displayln FAIL "\nError parsing input: " (error-message e) END)
      (flush-input (console-port))
      (console-input type name tag))))

(def (ask-parameter type surface-name)
  (console-input type surface-name #f))

(def (get-or-ask-parameters parameters program parameter-names)
  (displayln BOLD "Define parameters" END)
  (for ((name parameter-names))
    (def type (lookup-type program name))
    (def surface-name (lookup-surface-name program name))
    (get-or-ask
      parameters
      surface-name
      (lambda () (json<- type (ask-parameter type surface-name)))))
  (displayln)
  parameters)

(def (ask-role options role-names)
  (get-or-ask options
    'role
    (λ () (ask-option "Choose your role" role-names))))

;; --
;; Prompts participant for to select their identity (identified by their nickname)
;; from the runtime nickname-address table.
;; Returns their nickname as a string.
;; --
;; String <-
(def (prompt-identity)
  (ask-option
    "Choose your identity"
    (map
      (match <> ([nickname . address]
       (let (dependent-pair [Address . address])
         [nickname . dependent-pair])))
      (hash->list/sort address-by-nickname string<?))))

(def (ask-identity options)
  (get-or-ask options
    'identity
    (λ () (prompt-identity))))

(def (relative-to x y)
  (cond ((number? y) y)
        ((not (number? x)) (error "Expected a starting number"))
        ((not (string? y)) (error "Expected a string offset"))
        ((string-prefix? "+" y)
         ;; Positive offset from x.
         (+ x (.call Nat .<-string (substring/shared y 1 (string-length y)))))
        ((string-prefix? "-" y)
         ;; Negative offset from x.
         (- x (.call Nat .<-string (substring/shared y 1 (string-length y)))))
        ((string-prefix? "%" y)
         ;; Round x up to the nearest multiple of y.
         (let ((y (.call Nat .<-string (substring/shared y 1 (string-length y)))))
           (* (ceiling (/ x y)) y)))
        (else (.call Nat .<-string y))))

(def (ask-max-initial-block options current-block-number)
  (relative-to
   current-block-number
   (get-or-ask options
               'max-initial-block
               (λ ()
                 (def default-initial-block (number->string current-block-number))
                 (ask-number default-number: default-initial-block
                  (string-append
                   "Max initial block "
                   "[ Defaults to current block number: " default-initial-block " ]"))))))

(def (compile-contract contract.glow)
  (def compiler-output (run-passes contract.glow pass: 'project show?: #f))
  (parse-compiler-output compiler-output))

;; Takes an application identifier like "buy_sig#buySig" and returns
;; the part corresponding to a source file/path, e.g. "buy_sig"
(def (extract-application-source-path application-name)
  (match (pregexp-match "([^#]*)#?.*" application-name)
    ([_ dapp] dapp)
    (else (error "Bad application name" application-name))))

;; TODO: accept alternative ethereum networks, etc
;; TODO: Option spec should be able to take in parsers in option spec
;; to parse the options, using `getopt-parse`.
(define-entry-point (start-interaction
                     agreement: (agreement-json-string #f)
                     glow-app: (glow-app #f)
                     identity: (identity #f)
                     interaction: (interaction #f)
                     role: (role #f)
                     max-initial-block: (max-initial-block #f)
                     timeout-in-blocks: (timeout-in-blocks #f)
                     contacts: (contacts-file #f)
                     params: (params #f)
                     participants: (participants #f)
                     assets: (assets #f)
                     off-chain-channel-selection: (off-chain-channel-selection 'stdio)
                     tcp: (tcp-options #f)
                     host-address: (host-address "/ip4/0.0.0.0/tcp/10333")
                     circuit-relay-address: (circuit-relay-address #f)
                     pubsub-node: (pubsub-node #f)
                     wait-for-agreement: (wait-for-agreement #f))
  (help: "Start an interaction based on an agreement"
   getopt: (make-options
            [(option 'agreement "-A" "--agreement" default: #f
                     help: "interaction agreement as JSON")
             (option 'glow-app "-G" "--glow-app" default: #f
                     help: "the name of the Glow DApp")
             (option 'params "-P" "--params" default: #f
                     help: "contract parameters as JSON")
             (option 'participants "-p" "--participants" default: #f
                     help: "participant mapping as JSON")
             (option 'assets "-a" "--assets" default: #f
                     help: "asset mapping as JSON")
             ;; TODO: add an option for supplying single parameters/participants with
             ;; more ergonomic syntax than JSON, like --param foo=bar. We want to be
             ;; able to specify this mutliple times, which will require upstream
             ;; changes in gerbil's getopt.
             (option 'interaction "-I" "--interaction" default: #f
                     help: "path and name of interaction")
             (option 'identity "-M" "--my-identity" default: #f
                     help: "my identity for the interaction")
             (option 'role "-R" "--role" default: #f
                     help: "role to play in the interaction")
             (option 'max-initial-block "-B" "--max-initial-block" default: #f
                     help: "maximum block number the contract can begin at")
             (option 'timeout-in-blocks "-T" "--timeout-in-blocks" default: #f
                     help: "number of blocks after which to time out")
             ;; TODO: Abstract into Enum - See gerbil-poo
             ;; enum off-chain-channel = 'stdio | 'tcp | 'libp2p
             (option 'off-chain-channel-selection "-C" "--off-chain-channel" default: 'stdio
                     help: "command to specify off-chain-channel")
             (option 'tcp "--tcp" default: #f
                     help: "mapping of tcp addresses and ports as JSON")
             (option 'host-address "-O" "--host-address" default: "/ip4/0.0.0.0/tcp/10333"
                     help: "host-address for libp2p")
             (option 'circuit-relay-address "-d" "--circuit-relay-address" default: #f ;;TODO: Add the default circuit relay address here
                     help: "circuit-relay-address (only change if you want to use a non-default circuit relay)")
             (option 'pubsub-node "--pubsub-node" default: #f;; TODO: add the default pubsub node address here
                     help: "the address of a pubsub node (only change if you want to use a none-default node)")
             (flag 'wait-for-agreement "-W" "--wait-for-agreement"
                   help: "wait for agreement via off-chain-channel")]
            [(lambda (opt) (hash-remove! opt 'test))]
            [options/glow-path options/contacts
             options/evm-network options/database options/test options/backtrace]))

  ;; NOTE: This also populates the runtime nickname-address mapping table.
  (def contacts (load-contacts contacts-file))
  ;; TODO: Validate whether you possess the necessary keys to assume this identity.
  ;; TODO: Remove request for identity at stdio site.
  ;; 1. Update integration tests to answer prompts earlier,
  ;; 2. before agreements are generated / consumed (start-interaction/generate-agreement / start-interaction/with-agreement).
  ;; Once 1, 2 are done, we can remove the conditional for this.
  (def off-chain-channel-symbol (make-symbol off-chain-channel-selection))
  (def my-nickname
    (match off-chain-channel-symbol
      ('libp2p (or identity (prompt-identity)))
      (else identity)))

  (def options
       (hash
         (glow-app glow-app)
         (identity my-nickname)
         (params (string->json-object (or params "{}")))
         (participants (string->json-participant-map (or participants "{}")))
         (assets (string->json-asset-map (or assets "{}")))
         (max-initial-block max-initial-block)
         (timeout-in-blocks timeout-in-blocks)
         (role role)))

  ;; TODO: abstract into Poo object, especially if we have more local-runtime-options
  ;; TODO: error out if off-chain-channel-symbol is an invalid channel
  ;; FIXME: Mark off-chain-channel as experimental, print it in a console prompt
  (def channel-options
    (hash
     (off-chain-channel-selection off-chain-channel-symbol)
     (host-address host-address)
     (my-nickname my-nickname)
     (tcp-options (and tcp-options (json<-string tcp-options)))
     (contacts contacts)
     (circuit-relay-address circuit-relay-address)
     (pubsub-node pubsub-node)))
  (def off-chain-channel (init-off-chain-channel channel-options))
  (try
    ;; Start the interaction
    (displayln)
    (defvalues (agreement selected-role)
      (cond
        (wait-for-agreement
          (let (agreement (.call off-chain-channel .receive-agreement))
               (start-interaction/with-agreement options agreement)))
        (agreement-json-string
          (let (agreement (<-json InteractionAgreement (json<-string agreement-json-string)))
               (start-interaction/with-agreement options agreement)))
        (else (start-interaction/try-agreement options contacts off-chain-channel))))
    (def environment
      (let ((role (make-symbol selected-role)))
        (run role agreement off-chain-channel)))
    (displayln "Final environment:")
    ;; TODO: get run to include type t and pre-alpha-converted labels,
    ;; and output the entire thing as JSON omitting shadowed variables (rather than having conflicts)
    ;; TODO: highlight the returned value in the interaction, and have buy_sig return signature
    (for-each (match <> ([k t . v]
      (if (equal? (make-symbol k) 'signature)
        (display-object-ln BOLD k END " => " t v)
        (display-object-ln k " => " t v))))
      (hash->list/sort environment symbol<?))
    ;; Close channel once done / after erroring out
    (finally (.call off-chain-channel .close))))

(def (string->json-participant-map str)
  (def object (string->json-object str))
  (list->hash-table
    (hash-map
      (lambda (k v) (cons k (address<-0x v))) object)))

(def (string->json-asset-map str)
  (def object (string->json-object str))
  (list->hash-table
    (hash-map
      (lambda (k v) (cons k (lookup-asset (string->symbol v)))) object)))

;; TODO: Validate that selected role matches selected identity in the agreement's participant table.
(def (start-interaction/with-agreement options agreement)
  (let* ((selected-identity (ask-identity options))
         (interaction (.@ agreement interaction))
         (application.glow (find-dapp-file (extract-application-source-path interaction)))
         (program (compile-contract application.glow))
         (interaction-name (make-symbol (cadr (string-split interaction #\#))))
         (interaction-info (hash-get (.@ program interactions) interaction-name))
         (role-names (sort (filter identity (hash-keys (.@ interaction-info specific-interactions))) symbol<?))
         (selected-role (ask-role options role-names)))
  (values agreement selected-role)))

;; try receiving first, if no one's sending, then generate and send
(def (start-interaction/try-agreement options contacts off-chain-channel)
  (def agreement (.call off-chain-channel .try-receive-agreement))
  (cond
    (agreement (start-interaction/with-agreement options agreement))
    (else (start-interaction/generate-agreement options contacts off-chain-channel))))

(def (start-interaction/generate-agreement options contacts off-chain-channel)
  (displayln MAGENTA "Generating Agreement for other participants to join..." END)
  (nest
    (let (application-name
            (get-or-ask options 'glow-app (λ () (ask-application)))))
    (let (application-source-path (extract-application-source-path application-name)))
    (let (application.glow (find-dapp-file application-source-path)))
    (let (program (compile-contract application.glow)))
    (let (interaction-name
            (get-or-ask options 'interaction (λ () (ask-interaction (hash-keys (.@ program interactions)))))))
    (let (interaction-info (hash-get (.@ program interactions) interaction-name)))
    (let (role-names
            (sort (filter identity (hash-keys (.@ interaction-info specific-interactions))) symbol<?)))

    (let (selected-identity (ask-identity options)))
    (let (selected-role (ask-role options role-names)))

    (let (participants-table
           (get-or-ask-participants
             (hash-ref options 'participants)
             selected-identity
             (make-symbol selected-role)
             role-names
             contacts)))
    (let (network (ethereum-config)))
    (let (assets-table
          (get-or-ask-assets
            (hash-ref options 'assets)
            (.@ interaction-info asset-names)
            network)))
    (let (parameters
          (get-or-ask-parameters
            (hash-ref options 'params)
            program
            (.@ interaction-info parameter-names))))
    (let (timeout-in-blocks (hash-ref options 'timeout-in-blocks)))
    (let (current-block-number (eth_blockNumber)))
    (let (max-initial-block (ask-max-initial-block options current-block-number)))

    (let (blockchain-name (.@ network name))
      (for (a (hash-values assets-table))
        (unless (equal? network (asset->network a))
          (error "assets must all be on the same network"))))

    ;; TODO: Validate agreement, with nice user-friendly error message.
    (let (agreement
      {interaction: (string-append application-name "#" (symbol->string (.@ interaction-info name)))
      participants: (object<-hash participants-table)
      assets: (object<-hash assets-table)
      parameters
      glow-version: (software-identifier)
      code-digest: (digest<-file application.glow)
      reference: {}
      options: {blockchain: blockchain-name
                escrowAmount: (void)
                timeoutInBlocks:
                  (if timeout-in-blocks
                      (string->number timeout-in-blocks)
                      (* 100 (ethereum-timeout-in-blocks)))
                maxInitialBlock: max-initial-block}}))
    (begin
      (.call InteractionAgreement .validate agreement)
      ;; try-receive-agreement:
      (def other-agreement (.call off-chain-channel .try-receive-agreement))
      (cond 
        ;;  - yes: check match, FOR NOW: error on mismatch, TODO LATER: possible negotiations on failure
        (other-agreement
         (unless (equal? (json<- InteractionAgreement agreement)
                         (json<- InteractionAgreement other-agreement))
           (DDT "agreements-mismatch: generated agreement doesn't match received agreement"
             InteractionAgreement agreement
             InteractionAgreement other-agreement)
           (error "start-interaction/generate-agreement: generated agreement doesn't match received agreement" agreement other-agreement))
         (values agreement selected-role))
        ;;  - no: send now
        (else
         (.call off-chain-channel .send-agreement agreement)
         (values agreement selected-role))))))

;; UTILS
(def (flush-input port)
   (input-port-timeout-set! port 0.001)
   (let loop () (if (not (eof-object? (read-line port))) (loop)))
   (input-port-timeout-set! port +inf.0))

(export #t)

(import
  :gerbil/expander :gerbil/gambit/ports
  :std/format :std/generic :std/getopt :std/iter :std/misc/hash :std/misc/repr :std/misc/string :std/pregexp
  :std/sort :std/srfi/1 :std/srfi/13 :std/sugar :std/text/json
  :clan/base :clan/cli :clan/config :clan/exit :clan/filesystem :clan/hash :clan/json
  :clan/multicall :clan/path :clan/path-config :clan/string :clan/syntax
  :clan/poo/brace :clan/poo/cli :clan/poo/debug :clan/poo/object
  :clan/persist/db :clan/persist/content-addressing :clan/versioning :clan/pure/dict/symdict
  :mukn/ethereum/assets :mukn/ethereum/cli :mukn/ethereum/ethereum :mukn/ethereum/network-config
  :mukn/ethereum/types :mukn/ethereum/json-rpc :mukn/ethereum/known-addresses
  :mukn/glow/runtime/participant-runtime :mukn/glow/runtime/reify-contract-parameters
  :mukn/glow/runtime/program :mukn/glow/runtime/terminal-codes :mukn/glow/runtime/glow-path
  (only-in :mukn/glow/compiler/alpha-convert/alpha-convert init-syms)
  :mukn/glow/compiler/passes :mukn/glow/compiler/multipass :mukn/glow/compiler/syntax-context
  :mukn/glow/cli/contacts :mukn/glow/cli/identities)

(def (ask-option name options)
  (def options-count (length options))
  ;; TODO: Move this somewhere else, probably gerbil-utils?
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
  (let* ((input (read-line))
         (response (string->number input)))
    (if (<= 1 response options-count)
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

(def (ask-string name)
  (display-prompt name)
  (def result (read-line))
  (displayln)
  result)

(def (ask-number name)
  (display-prompt name)
  (def result (.call Nat .<-string (read-line)))
  (displayln)
  result)

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

(def (print-command agreement)
  (displayln MAGENTA "One line command for other participants to generate the same agreement:" END)
  (display "glow start-interaction --agreement ")
  (def agreement-string (string<-json (json<- InteractionAgreement agreement)))
  (if (string-contains agreement-string "'")
    (pr agreement-string)
    (display (string-append "'" agreement-string "'")))
  (displayln)
  (force-output))

(def (get-or-ask options option ask-function)
  (if-let (option-value (hash-get options option))
    option-value
    (let (input (apply ask-function []))
      (hash-put! options option input)
      input)))

(def (ask-role options role-names)
  (get-or-ask options
    'role
    (λ () (ask-option "Choose your role" role-names))))

(def (ask-identity options)
  (get-or-ask options
    'identity
    (λ () (ask-option
            "Choose your identity"
            (map (match <>
                  ([nickname . address]
                    (let (dependent-pair [Address . address])
                      [nickname . dependent-pair])))
                (hash->list/sort address-by-nickname string<?))))))

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
                 (ask-number
                  (string-append
                   "Max initial block "
                   "[ Current block number is " (number->string current-block-number) " ]"))))))

(def (compile-contract contract.glow)
  (def compiler-output (run-passes contract.glow pass: 'project show?: #f))
  (parse-compiler-output compiler-output))

;; Takes an application identifier like "buy_sig#payForSignature" and returns
;; the part corresponding to a source file/path, e.g. "buy_sig"
(def (extract-application-source-path application-name)
  (match (pregexp-match "([^#]*)#?.*" application-name)
    ([_ dapp] dapp)
    (else (error "Bad application name" application-name))))

;; TODO: accept alternative ethereum networks, etc
(define-entry-point (start-interaction
                     agreement: (agreement-json-string #f)
                     glow-app: (glow-app #f)
                     identity: (identity #f)
                     interaction: (interaction #f)
                     role: (role #f)
                     max-initial-block: (max-initial-block #f)
                     timeout-in-blocks: (timeout-in-blocks #f)
                     contacts: (contacts-file #f)
                     handshake: (handshake #f)
                     params: (params #f)
                     participants: (participants #f))
  (help: "Start an interaction based on an agreement"
   getopt: (make-options
            [(option 'agreement "-A" "--agreement" default: #f
                     help: "interaction agreement as JSON")
             (option 'glow-app "-G" "--glow-app" default: #f
                     help: "the name of the Glow DApp")
             ;; TODO: add an option for supplying single parameters/participants with
             ;; more ergonomic syntax than JSON, like --param foo=bar. We want to be
             ;; able to specify this mutliple times, which will require upstream
             ;; changes in gerbil's getopt.
             (option 'params "-P" "--params" default: #f
                     help: "contract parameters as JSON")
             (option 'participants "-p" "--participants" default: #f
                     help: "participant mapping as JSON")
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
             (option 'handshake "-H" "--handshake" default: #f
                     help: "command to use to transfer handshakes")]
            [(lambda (opt) (hash-remove! opt 'test))]
            [options/glow-path options/contacts
             options/evm-network options/database options/test options/backtrace]))
  (def options
       (hash
         (glow-app glow-app)
         (identity identity)
         (params (string->json-object (or params "{}")))
         (participants (string->json-participant-map (or participants "{}")))
         (max-initial-block max-initial-block)
         (timeout-in-blocks timeout-in-blocks)
         (role role)))
  (displayln)
  (def contacts (load-contacts contacts-file))
  (defvalues (agreement selected-role)
    (if agreement-json-string
      (start-interaction/with-agreement options (<-json InteractionAgreement (json<-string agreement-json-string)))
      (start-interaction/generate-agreement options contacts)))
  (def environment
    (let ((role (symbolify selected-role)))
      (if handshake
        (run:command ["/bin/sh" "-c" handshake] role agreement)
        (run:terminal role agreement))))
  (displayln "Final environment:")
  ;; TODO: get run to include type t and pre-alpha-converted labels,
  ;; and output the entire thing as JSON omitting shadowed variables (rather than having conflicts)
  ;; TODO: highlight the returned value in the interaction, and have buy_sig return signature
  (for-each (match <> ([k t . v]
              (if (equal? (symbolify k) 'signature)
                (display-object-ln BOLD k END " => " t v)
                (display-object-ln k " => " t v))))
            (hash->list/sort environment symbol<?)))

(def (string->json-participant-map str)
  (def object (string->json-object str))
  (list->hash-table
    (hash-map
      (lambda (k v) (cons k (address<-0x v))) object)))

;; TODO: Validate that selected role matches selected identity in the agreement's participant table.
(def (start-interaction/with-agreement options agreement)
  (let* ((selected-identity (ask-identity options))
         (interaction (.@ agreement interaction))
         (application.glow (find-dapp-file (extract-application-source-path interaction)))
         (program (compile-contract application.glow))
         (interaction-name (symbolify (cadr (string-split interaction #\#))))
         (interaction-info (hash-get (.@ program interactions) interaction-name))
         (role-names (sort (filter identity (hash-keys (.@ interaction-info specific-interactions))) symbol<?))
         (selected-role (ask-role options role-names)))
  (values agreement selected-role)))

(def (start-interaction/generate-agreement options contacts)
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
             (symbolify selected-role)
             role-names
             contacts)))
    (let (parameters
          (get-or-ask-parameters
            (hash-ref options 'params)
            program
            (.@ interaction-info parameter-names))))
    (let (timeout-in-blocks (hash-ref options 'timeout-in-blocks)))
    (let (current-block-number (eth_blockNumber)))
    (let (max-initial-block (ask-max-initial-block options current-block-number)))

    ;; TODO: Validate agreement, with nice user-friendly error message.
    (let (agreement
      {interaction: (string-append application-name "#" (symbol->string (.@ interaction-info name)))
      participants: (object<-hash participants-table)
      parameters
      glow-version: (software-identifier)
      code-digest: (digest<-file application.glow)
      reference: {}
      options: {blockchain: (.@ (ethereum-config) name)
                escrowAmount: (void)
                timeoutInBlocks:
                  (if timeout-in-blocks
                      (string->number timeout-in-blocks)
                      (* 100 (ethereum-timeout-in-blocks)))
                maxInitialBlock: max-initial-block}}))
    (begin
      (.call InteractionAgreement .validate agreement)
      (print-command agreement)
      (values agreement selected-role))))

;; UTILS
(def (flush-input port)
   (input-port-timeout-set! port 0.001)
   (let loop () (if (not (eof-object? (read-line port))) (loop)))
   (input-port-timeout-set! port +inf.0))

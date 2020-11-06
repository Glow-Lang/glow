; Interfaces to the Plutus Platform's smart contract backend.
; CLI interface: https://github.com/input-output-hk/plutus/blob/1e947805c4cc4e41b149e23d529cca0c9fd79485/plutus-scb/app/Main.hs
; HTTP interface: https://github.com/input-output-hk/plutus/blob/1e947805c4cc4e41b149e23d529cca0c9fd79485/plutus-scb/src/Plutus/SCB/Webserver/API.hs
(export #t)

(import
  :clan/exception :clan/logger
  :gerbil/gambit/ports :gerbil/gambit/threads
  :std/net/request :std/text/json :std/misc/ports :std/misc/process :std/misc/repr
  :clan/poo/poo :clan/poo/brace
  ./util ./haskell-types/client)

; TODO: Use object->string on sexpressions. Possibly sexp<- in POO objects?
(def (glow-contract:create uuid header body variable-map)
  (let (params {sourceHeader: (object->string header)
                sourceBody: (object->string body)
                initialVariableMap: (object->string variable-map)
                timeoutLength: 100})
    (update-contract uuid "create"
      (json-object->string (.call CreateParams .json<- params)))))

(def (glow-contract:move uuid variable-map entry-point)
  (let (params {variableMap: (object->string variable-map)
                entryPoint: entry-point})
    (update-contract uuid "move"
      (json-object->string (.call MoveParams .json<- params)))))

(def (glow-contract:wait uuid)
  (update-contract uuid "wait" "[]"))

(def (glow-contract:extract uuid variable-name)
  (contract-state uuid))

; Update the database with the latest schema.
(def (run-migrate)
  (run-plutus-scb-server "migrate"))

; Run all the mock servers needed.
(def (run-all-servers)
  (run-plutus-scb-server "all-servers"))

; Run a mock version of the Cardano node API server.
(def (run-node-server)
  (run-plutus-scb-server "node-server"))

; Run a mock version of the Cardano wallet API server.
(def (run-wallet-server)
  (run-plutus-scb-server "wallet-server"))

; Run the chain index.
(def (run-chain-index)
  (run-plutus-scb-server "chain-index"))

; Run the signing process.
(def (run-signing-process)
  (run-plutus-scb-server "signing-process"))

; Install a new smart contract.
(def (install-contract path)
  (run-contract-command "install" ["--path" path]))

; Show all installed contracts.
(def (installed-contracts)
  (run-contract-command "installed" []))

; Activate a smart contract.
(def (activate-contract path)
  (let
    (contract (webserver-post
      (make-url "/api/contract/activate")
      (list->json-string (list ["contractPath" . path]))))
    (hash-get (hash-get contract 'csContract) 'unContractInstanceId)))

(def (activate-contract-cli path)
  (run-contract-command "activate" ["--path" path]))

; Show all active contracts.
(def (active-contracts)
  (run-contract-command "active" []))

; Update a smart contract.
(def (update-contract uuid endpoint json)
  (webserver-post
    (make-url (string-append "/api/contract/" uuid "/endpoint/" endpoint))
    json))

(def (update-contract-cli uuid endpoint json)
  (run-contract-command "update" [uuid endpoint json]))

; Show the current state of a contract.
(def (contract-state uuid)
  (run-contract-command "state" [uuid]))

; Show the state history of a contract.
(def (contract-history uuid)
  (run-contract-command "history" [uuid]))

; Process the inbox of the contract instance.
(def (process-contract-inbox uuid)
  (run-contract-command "process-inbox" [uuid]))

; Process all contract outboxes.
(def (process-contract-outboxes)
  (run-contract-command "process-outboxes" []))

(def scb-logger (json-run-logger "scb"))

(def (make-url path)
  (string-append base-url path))

(defvalues (base-url)
  (values "127.0.0.1:8080"))

; Helpers
(def (run-contract-command cmd args)
  (run-plutus-scb-command (append ["contracts" cmd] args)))

(def (run-plutus-scb-server cmd)
  (let ((config-file "plutus-scb.yaml")
        (log-config-file "plutus-scb-logging.yaml"))
    (run-process
      ["plutus-scb" cmd
       "--config" config-file
       "--log-config" log-config-file])))

; (call-with-output-to-file)
; gerbil/gambit/ports
; (open-file)
; std/misc/port

(def (run-plutus-scb-command cmd)
  (run-process (cons "plutus-scb" cmd)
    stdout-redirection: #f
    stderr-redirection: #f
    check-status: (lambda (_ _) #t)))

(def (to-hash-table obj)
  (let (fields (make-pairs (cdr (class->list obj))))
    (list->hash-table
      (map
        (lambda (pair)
          (if (object? (cdr pair))
            (cons (car pair) (to-hash-table (cdr pair)))
            pair))
        fields))))

(def (make-pairs a-list)
  (match a-list
    ([a b rest ...] (cons [a . b] (make-pairs rest)))
    (else [])))


; (def (post-transaction transaction)
;   (let (data (json-object->string transaction))
;     (node-post "/mempool" data)))
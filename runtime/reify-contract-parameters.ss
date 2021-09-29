(export run run:terminal run:special-file run:command)
(import
  :std/format :std/iter :std/pregexp :std/misc/string :std/text/json
  :clan/debug :clan/ffi :clan/json
  :clan/poo/object :clan/poo/mop :clan/poo/debug
  :clan/path-config :clan/pure/dict/symdict
  :gerbil/gambit/ports
  :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/glow/compiler/syntax-context :mukn/glow/compiler/multipass :mukn/glow/compiler/passes
  (only-in ../compiler/alpha-convert/env symbol-refer)
  ./program ./participant-runtime ./terminal-codes ./glow-path)

(def (write-json-handshake handshake port)
  (write-json-ln (json<- AgreementHandshake handshake) port)
  (force-output port))

(def (read-json-handshake port)
  (def handshake-json (json<-port port))
  (when (eof-object? handshake-json)
    (error "read-json-handshake: expected JSON, got EOF from port" port))
  (<-json AgreementHandshake handshake-json))

(.def io-context:terminal
  setup:
  (lambda () (assert! (input-port? (current-input-port))))
  teardown:
  void
  send-handshake:
  (lambda (handshake)
    (displayln MAGENTA "\nSend the handshake below to the other participant:" END)
    (write-json-handshake handshake (current-output-port)))
  receive-handshake:
  (lambda ()
    (displayln MAGENTA "\nPaste below the handshake sent by the other participant:" END)
    (read-json-handshake (current-input-port))))

(def (io-context:command cmd)
  (def proc (open-process
              (list
                path: (car cmd)
                arguments: (cdr cmd))))
  (.o
    (teardown
      (lambda ()
        (close-port proc)
        (kill (process-pid proc))
        (process-status proc)))
    (send-handshake
      (lambda (handshake)
        (write-json-handshake handshake proc)))
    (receive-handshake
      (lambda ()
        (read-json-handshake proc)))))


;; interaction-agreement->program : InteractionAgreement -> Program
(def (interaction-agreement->program a)
  (defvalues (modpath name) (split-interaction-path-name (.@ a interaction)))
  (def path (find-dapp-file modpath))
  (def compiler-output (run-passes path pass: 'project show?: #f))
  (parse-compiler-output compiler-output))

;; A TypeValuePair is a dependent pair of a type and a value of that type:
;;   (Σ (value : Type) value)

;; surface-name-environment : AlphaEnv [Hashof Symbol V] -> [Hashof Symbol V]
;; Takes an environment that uses alpha-names, produces one
;; that uses surface-names instead, with the same values
(def (surface-name-environment alpha env)
  (def new (make-hash-table-eq))
  (for ((surface-name (symdict-keys alpha)))
    (def alpha-name (symbol-refer alpha surface-name))
    (when (hash-key? env alpha-name)
      (hash-put! new surface-name (hash-get env alpha-name))))
  new)

;; program-environment-type-value-pairs :
;; Program Environment -> [Hashof Symbol TypeValuePair]
;; The symbols are alpha-names, not surface names
(def (program-environment-type-value-pairs prg env)
  (list->hash-table
   (for/collect ((ent (hash->list env)))
     (match ent
       ((cons k v)
        (cons k (cons (lookup-type prg k) v)))))))

(def (run:terminal role a off-chain-channel)
  (run io-context:terminal role a off-chain-channel))

(def (run:command cmd role a off-chain-channel)
  (def ctx (io-context:command cmd))
  (with-unwind-protect
    (lambda () (run ctx role a off-chain-channel))
    (lambda () (.call ctx teardown)))) ;; FIXME: teardown off-chain-channel here also

(def (run:special-file role a off-chain-channel)
  (run io-context:special-file role a off-chain-channel))

;; run : Symbol InteractionAgreement -> [Hashof Symbol TypeValuePair]
;; Produces an environment mapping surface names to type-value-pairs
(def (run ctx role a off-chain-channel)
  (def program (interaction-agreement->program a))
  (def runtime
    (.call Runtime .make role: role
                  agreement: a
                  program: program
                  io-context: ctx
                  off-chain-channel: off-chain-channel))
  (execute runtime)
  (printf "~a~a interaction finished~a\n" BOLD (.@ a interaction) END)
  (surface-name-environment
   (hash-get (hash-get (.@ program compiler-output) 'DebugLabelTable)
             (.@ runtime current-debug-label))
   (program-environment-type-value-pairs program (.@ runtime environment))))


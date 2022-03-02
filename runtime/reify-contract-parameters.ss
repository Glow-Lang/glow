(export run)
(import
  :std/format :std/iter :std/pregexp :std/misc/string :std/text/json
  :clan/debug :clan/ffi :clan/json
  :clan/poo/object :clan/poo/mop :clan/poo/debug
  :clan/path-config :clan/pure/dict/symdict
  :gerbil/gambit/ports
  :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/glow/compiler/syntax-context :mukn/glow/compiler/multipass :mukn/glow/compiler/passes
  (only-in ../compiler/alpha-convert/env symbol-refer)
  (only-in ../compiler/common hash-kref)
  ./program ./participant-runtime ./terminal-codes ./glow-path)

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

;; run : Symbol InteractionAgreement OffChainChannel -> [Hashof Symbol TypeValuePair]
;; Produces an environment mapping surface names to type-value-pairs
(def (run role a off-chain-channel)
  (def program (interaction-agreement->program a))
  (def runtime
    (.call Runtime .make role: role
                  agreement: a
                  program: program
                  off-chain-channel: off-chain-channel))
  (execute runtime)
  (printf "~a~a interaction finished~a\n" BOLD (.@ a interaction) END)
  (unless (symbol? (.@ runtime current-debug-label))
    (error "expected a symbol for runtime current-debug-label, given:" (.@ runtime current-debug-label)))
  (surface-name-environment
   (hash-kref (hash-kref (.@ program compiler-output) 'DebugLabelTable)
              (.@ runtime current-debug-label))
   (program-environment-type-value-pairs program (.@ runtime environment))))


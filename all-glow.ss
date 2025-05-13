;; -*- Gerbil -*-
;; Use this file by include'ing it in your gxi REPL:
;;   (eval `(include ,(path-expand "all-glow.ss" (getenv "GLOW_SRC"))))
;; Or if you prefer without eval, but computing the absolute path yourself, something like:
;;   (include "~/src/glow/all-glow.ss")
;; Or just run ./ggxi

;; TODO: maybe have build.ss extract the list of files from this file (?)

(import

  ;; Gerbil
  :gerbil/gambit
  :gerbil/expander
  :gerbil/expander
  :std/interactive
  :std/actor
  :std/cli/getopt
  :std/cli/multicall
  :std/cli/print-exit
  :std/cli/shell
  :std/assert
  :std/coroutine
  :std/debug/DBG
  :std/error
  :std/format
  :std/io
  :std/iter
  :std/logger
  :std/misc/bytes
  :std/misc/decimal
  :std/misc/deque
  :std/misc/evector
  :std/misc/hash
  :std/misc/list
  :std/misc/number
  :std/misc/path
  :std/misc/ports
  :std/misc/process
  :std/misc/queue
  :std/misc/repr
  :std/misc/string
  :std/misc/vector
  :std/net/request
  :std/parser/ll1
  :std/pregexp
  :std/sort
  :std/source
  :std/srfi/1 ;; lists
  :std/srfi/13 ;; strings
  (except-in :std/srfi/19 time)
  :std/srfi/133 ;; vectors (instead of 43)
  :std/srfi/141 ;; integer division
  :std/stxutil
  :std/sugar
  :std/text/basic-printers
  :std/text/char-set
  :std/text/csv
  :std/text/hex
  :std/text/json
  :std/test

  ;; swank
  ;; NB: until https://github.com/ecraven/r7rs-swank/pull/10 is merged,
  ;; use https://github.com/fare-patches/r7rs-swank as your checkout
  ;; :ecraven/gerbil-swank

  ;; gerbil-utils
  :clan/net/tcp
  :clan/net/websocket
  :clan/net/whois
  :clan/assert
  :clan/base
  #;:clan/call-limiter
  :clan/cli
  :clan/concurrency
  :clan/config
  :clan/diceware
  :clan/error
  :clan/failure
  :clan/ffi
  :clan/files
  :clan/filesystem
  :clan/generator
  :clan/git-fu
  :clan/hash
  :clan/json
  :clan/list
  :clan/logger
  :clan/maybe
  :clan/memo
  :clan/option
  :clan/order
  :clan/path-config
  :clan/peekable-iterator
  :clan/ports
  :clan/random
  :clan/simple-actor-client
  :clan/source
  :clan/stateful-avl-map
  :clan/string
  :clan/syntax
  :clan/temporary-files
  :clan/timestamp
  :clan/versioning
  :clan/watch

  ;; gerbil-poo
  :clan/poo/cli
  :clan/poo/debug
  :clan/poo/object
  :clan/poo/io
  :clan/poo/trie
  (prefix-in :clan/poo/mop poo.)
  (prefix-in :clan/poo/type poo.)
  (only-in :clan/poo/mop
           Type Type. Class Class. Slot Lens Function Fun
           .defgeneric .method proto validate element? slot-lens sexp<-)
  (only-in :clan/poo/number Number Real JsInt IntSet)

  ;; gerbil-crypto
  :clan/crypto/keccak
  :clan/crypto/secp256k1

  ;; gerbil-persist
  :clan/persist/content-addressing
  :clan/persist/db
  :clan/persist/persist
  :clan/persist/merkle-trie

  ;; gerbil-ethereum
  :clan/ethereum/abi
  :clan/ethereum/assembly
  :clan/ethereum/assets
  :clan/ethereum/cli
  :clan/ethereum/contract-config
  :clan/ethereum/erc20
  :clan/ethereum/ethereum
  :clan/ethereum/evm-runtime
  :clan/ethereum/hex
  :clan/ethereum/json-rpc
  :clan/ethereum/known-addresses
  :clan/ethereum/logger
  :clan/ethereum/meta-create2
  :clan/ethereum/network-config
  :clan/ethereum/nonce-tracker
  :clan/ethereum/presigned
  :clan/ethereum/rlp
  :clan/ethereum/simple-apps
  ;;:clan/ethereum/testing
  :clan/ethereum/transaction
  :clan/ethereum/tx-tracker
  :clan/ethereum/types
  :clan/ethereum/watch

  ;; Glow
  :mukn/glow/cli/contacts
  :mukn/glow/cli/ethereum
  :mukn/glow/cli/identities
  :mukn/glow/cli/interaction
  :mukn/glow/compiler/alpha-convert/alpha-convert
  :mukn/glow/compiler/alpha-convert/symbolnat
  :mukn/glow/compiler/alpha-convert/fresh
  :mukn/glow/compiler/anf/anf
  ;;:mukn/glow/compiler/checkpointify/checkpointify
  :mukn/glow/compiler/common
  :mukn/glow/compiler/desugar/desugar
  :mukn/glow/compiler/multipass
  :mukn/glow/compiler/passes
  :mukn/glow/compiler/syntax-context
  :mukn/glow/compiler/typecheck/typecheck
  :mukn/glow/path-config
  :mukn/glow/runtime/consensus-code-generator
  :mukn/glow/runtime/glow-path
  :mukn/glow/runtime/participant-runtime
  :mukn/glow/version

  ;; Testing Glow
  :mukn/glow/t/common
  )

(import :clan/poo/brace)

;; COPY THE LINE BELOW to files you try to debug
(import :clan/debug)

;;(extern namespace: #f add-load-path) (add-load-path (application-source-directory))

;;(displayln "Welcome, Glow hacker")

;;(##set-debug-settings! 15 3) ;; TODO: remember what this is about

;; Force the stdout port width to 218… the port width is used by the pretty printer to determine when to break lines… the Gambit rtlib should export a higher level API than that hack
(##vector-set! ##stdout-port 37 (lambda (port) 218))

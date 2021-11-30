;; -*- Gerbil -*-
;; Use this file by include'ing it in your gxi REPL:
;;   (eval `(include ,(path-expand "all-glow.ss" (getenv "GLOW_SRC"))))
;; Or if you prefer without eval, but computing the absolute path yourself, something like:
;;   (include "~/src/glow/all-glow.ss")
;; Or just run ./ggxi

;; TODO: maybe have build.ss extract the list of files from this file (?)

(import

  ;; Gerbil
  (only-in :gerbil/gambit/os time)
  :gerbil/expander <expander-runtime> :std/interactive
  :gerbil/gambit/bytes :gerbil/gambit/exact :gerbil/gambit/threads :gerbil/gambit/ports
  :scheme/base-impl :scheme/char
  :std/actor :std/coroutine
  :std/debug/heap :std/debug/memleak :std/debug/threads
  :std/error :std/format :std/getopt :std/iter :std/logger
  :std/misc/bytes :std/misc/deque :std/misc/hash :std/misc/list :std/misc/number
  :std/misc/ports :std/misc/process :std/misc/queue :std/misc/repr :std/misc/string
  :std/net/request :std/net/websocket
  :std/pregexp :std/sort
  :std/srfi/1 :std/srfi/13 (except-in :std/srfi/19 time) ;; :std/srfi/43
  :std/sugar :std/assert :std/text/csv :std/text/hex :std/text/json :std/test

  ;; swank
  ;; NB: until https://github.com/ecraven/r7rs-swank/pull/10 is merged,
  ;; use https://github.com/fare-patches/r7rs-swank as your checkout
  ;; :ecraven/gerbil-swank

  ;; Clan Utilities
  :clan/net/json-rpc :clan/net/s3 :clan/net/simple-http-client :clan/net/websocket
  :clan/assert :clan/base :clan/basic-parsers :clan/basic-printers
  #;:clan/call-limiter :clan/cli :clan/concurrency :clan/config
  :clan/debug :clan/decimal :clan/diceware
  :clan/error :clan/exit
  :clan/failure :clan/ffi :clan/files :clan/filesystem
  :clan/generator :clan/git-fu :clan/hash
  :clan/json :clan/list :clan/logger
  :clan/maybe :clan/memo :clan/multicall :clan/number :clan/option :clan/order
  :clan/path :clan/path-config :clan/peekable-iterator :clan/ports
  :clan/random
  :clan/shell :clan/simple-rpc-client :clan/source
  :clan/stateful-avl-map :clan/string :clan/subprocess :clan/syntax
  :clan/temporary-files :clan/timestamp
  :clan/vector :clan/versioning :clan/watch :clan/with-id
  :clan/persist/db :clan/persist/persist :clan/persist/content-addressing

  ;; POO
  :clan/poo/object :clan/poo/io :clan/poo/debug :clan/poo/cli
  (prefix-in :clan/poo/mop poo.) (prefix-in :clan/poo/type poo.)
  (only-in :clan/poo/mop
           Type Type. Class Class. Slot Lens Function Fun
           .defgeneric .method proto validate element? slot-lens sexp<-)
  (only-in :clan/poo/number Number Real JsInt IntSet)
  :clan/poo/trie

  ;; Glow
  :mukn/glow/version
  :mukn/glow/path-config
  :mukn/glow/compiler/syntax-context
  :mukn/glow/compiler/passes :mukn/glow/compiler/multipass :mukn/glow/compiler/common
  :mukn/glow/compiler/alpha-convert/symbolnat :mukn/glow/compiler/alpha-convert/fresh
  :mukn/glow/compiler/alpha-convert/alpha-convert
  :mukn/glow/compiler/desugar/desugar
  :mukn/glow/compiler/typecheck/typecheck
  :mukn/glow/compiler/anf/anf
  :mukn/glow/runtime/participant-runtime
  :mukn/glow/runtime/consensus-code-generator
  :mukn/glow/runtime/glow-path
  :mukn/glow/cli/interaction :mukn/glow/cli/contacts :mukn/glow/cli/identities :mukn/glow/cli/ethereum

  ;;:mukn/glow/compiler/checkpointify/checkpointify
  :clan/crypto/keccak :clan/crypto/secp256k1
  :mukn/ethereum/hex :mukn/ethereum/abi :mukn/ethereum/types :mukn/ethereum/ethereum
  :mukn/ethereum/rlp :mukn/ethereum/known-addresses :mukn/ethereum/logger
  :mukn/ethereum/network-config :mukn/ethereum/json-rpc :mukn/ethereum/transaction :mukn/ethereum/watch
  :mukn/ethereum/assembly :mukn/ethereum/contract-config :mukn/ethereum/evm-runtime
  :mukn/ethereum/nonce-tracker :mukn/ethereum/tx-tracker :mukn/ethereum/simple-apps
  :mukn/ethereum/assets :mukn/ethereum/erc20 :mukn/ethereum/cli :mukn/ethereum/meta-create2

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

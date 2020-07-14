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
  :std/misc/bytes :std/misc/list :std/misc/ports :std/misc/process :std/misc/repr :std/misc/string
  :std/net/websocket
  :std/pregexp :std/sort
  :std/srfi/1 :std/srfi/13 (except-in :std/srfi/19 time) ;; :std/srfi/43
  :std/sugar :std/text/csv :std/text/hex :std/text/json :std/test

  ;; swank
  ;; NB: until https://github.com/ecraven/r7rs-swank/pull/10 is merged,
  ;; use https://github.com/fare-patches/r7rs-swank as your checkout
  ;; :ecraven/gerbil-swank

  ;; Clan Utilities
  :utils/net/json-rpc.net :utils/net/s3 :utils/net/simple-http-client :utils/net/websocket
  :utils/assert :utils/base :utils/basic-parsers :utils/basic-printers
  :utils/call-limiter :utils/concurrency :utils/config
  :utils/timestamp :utils/debug :utils/diceware
  :utils/error :utils/exit
  :utils/failure :utils/ffi :utils/files :utils/filesystem
  :utils/generator :utils/hash
  :utils/json :utils/list :utils/logger
  :utils/maybe :utils/memo :utils/multicall :utils/number :utils/option
  :utils/path :utils/path-config :utils/peekable-iterator
  :utils/random
  :utils/source :utils/simple-rpc-client :utils/stateful-avl-map :utils/string
  :utils/subprocess
  :utils/temporary-files
  :utils/vector :utils/versioning :utils/watch
  :persist/db :persist/persist
  :utils/pure/dict/intdict

  ;; POO
  :poo/poo :poo/io
  (prefix-in :poo/mop poo.) (prefix-in :poo/type poo.)
  (only-in :poo/mop
           Type Type. Class Class. Slot Lens Function Fun
           .defgeneric .method proto validate element? slot-lens sexp<-)
  (only-in :poo/number Number Real JsInt IntSet)

  ;; Versions for dependencies
  :utils/version :crypto/version :poo/version :persist/version :ethereum/version :glow/version

  ;; Glow
  (for-template :glow/compiler/syntax-context)
  :glow/config/path
  :glow/compiler/syntax-context
  :glow/compiler/passes :glow/compiler/multipass :glow/compiler/common
  :glow/compiler/alpha-convert/symbolnat :glow/compiler/alpha-convert/fresh
  :glow/compiler/alpha-convert/alpha-convert
  :glow/compiler/desugar/desugar
  :glow/compiler/typecheck/typecheck
  :glow/compiler/anf/anf
  ;;:glow/compiler/checkpointify/checkpointify
  :crypto/keccak :crypto/secp256k1
  :ethereum/hex :ethereum/abi :ethereum/types :ethereum/ethereum
  :ethereum/signing :ethereum/known-addresses
  :ethereum/network-config :ethereum/json-rpc :ethereum/transaction
  :ethereum/assembly :ethereum/contract-config :ethereum/contract-support
  :ethereum/nonce-tracker :ethereum/tx-tracker :ethereum/batch-send

  ;; Testing Glow
  :glow/t/common
  ;:glow/compiler/t/multipass-test
  ;:glow/compiler/alpha-convert/t/alpha-convert-test
  ;:glow/compiler/anf/t/anf-test
)

(import :poo/brace)

;; COPY THE LINE BELOW to files you try to debug
(import :utils/debug)

;;(extern namespace: #f add-load-path) (add-load-path (glow-src))

;;(printf "Welcome, Glow hacker\n")

;;(##set-debug-settings! 15 3)
(##vector-set! ##stdout-port 37 (lambda (port) 218))

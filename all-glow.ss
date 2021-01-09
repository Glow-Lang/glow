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
  :std/sugar :std/text/csv :std/text/hex :std/text/json :std/test

  ;; swank
  ;; NB: until https://github.com/ecraven/r7rs-swank/pull/10 is merged,
  ;; use https://github.com/fare-patches/r7rs-swank as your checkout
  ;; :ecraven/gerbil-swank

  ;; Clan Utilities
  :clan/net/json-rpc :clan/net/s3 :clan/net/simple-http-client :clan/net/websocket
  :clan/assert :clan/base :clan/basic-parsers :clan/basic-printers
  :clan/call-limiter :clan/concurrency :clan/config
  :clan/timestamp :clan/debug :clan/diceware
  :clan/error :clan/exit
  :clan/failure :clan/ffi :clan/files :clan/filesystem
  :clan/generator :clan/git-fu :clan/hash
  :clan/json :clan/list :clan/logger
  :clan/maybe :clan/memo :clan/multicall :clan/number :clan/option :clan/order
  :clan/path :clan/path-config :clan/peekable-iterator :clan/ports
  :clan/random
  :clan/source :clan/simple-rpc-client :clan/stateful-avl-map :clan/string
  :clan/subprocess :clan/syntax
  :clan/temporary-files
  :clan/vector :clan/versioning :clan/watch :clan/with-id
  :clan/persist/db :clan/persist/persist :clan/persist/content-addressing

  ;; POO
  :clan/poo/poo :clan/poo/io :clan/poo/debug
  (prefix-in :clan/poo/mop poo.) (prefix-in :clan/poo/type poo.)
  (only-in :clan/poo/mop
           Type Type. Class Class. Slot Lens Function Fun
           .defgeneric .method proto validate element? slot-lens sexp<-)
  (only-in :clan/poo/number Number Real JsInt IntSet)
  :clan/poo/trie

  ;; Glow
  (for-template :mukn/glow/compiler/syntax-context)
  :mukn/glow/version
  :mukn/glow/path-config
  :mukn/glow/compiler/syntax-context
  :mukn/glow/compiler/passes :mukn/glow/compiler/multipass :mukn/glow/compiler/common
  :mukn/glow/compiler/alpha-convert/symbolnat :mukn/glow/compiler/alpha-convert/fresh
  :mukn/glow/compiler/alpha-convert/alpha-convert
  :mukn/glow/compiler/desugar/desugar
  :mukn/glow/compiler/typecheck/typecheck
  :mukn/glow/compiler/anf/anf
  :mukn/glow/runtime/ethereum-contract
  :mukn/glow/runtime/ethereum-runtime

  ;;:mukn/glow/compiler/checkpointify/checkpointify
  :clan/crypto/keccak :clan/crypto/secp256k1
  :mukn/ethereum/hex :mukn/ethereum/abi :mukn/ethereum/types :mukn/ethereum/ethereum
  :mukn/ethereum/rlp :mukn/ethereum/signing :mukn/ethereum/known-addresses :mukn/ethereum/logger
  :mukn/ethereum/network-config :mukn/ethereum/json-rpc :mukn/ethereum/transaction :mukn/ethereum/watch
  :mukn/ethereum/assembly :mukn/ethereum/contract-config :mukn/ethereum/contract-runtime
  :mukn/ethereum/nonce-tracker :mukn/ethereum/tx-tracker :mukn/ethereum/batch-send
  :mukn/ethereum/assets


  ;; Testing Glow
  :mukn/glow/t/common
  ;:mukn/glow/compiler/t/multipass-test
  ;:mukn/glow/compiler/alpha-convert/t/alpha-convert-test
  ;:mukn/glow/compiler/anf/t/anf-test
)

(import :clan/poo/brace)

;; COPY THE LINE BELOW to files you try to debug
(import :clan/debug)

;;(extern namespace: #f add-load-path) (add-load-path (glow-src))

;;(printf "Welcome, Glow hacker\n")

;;(##set-debug-settings! 15 3)
(##vector-set! ##stdout-port 37 (lambda (port) 218))

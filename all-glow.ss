;; -*- Gerbil -*-
#| Use this file by include'ing it in your gxi REPL:
(eval `(include ,(path-expand "all-glow.ss" (getenv "GLOW_SRC"))))
;; Or if you prefer without eval, but computing the absolute path yourself, something like:
(include "~/src/glow/all-glow.ss")
;; Or just run ./ggxi
|#

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
  :clan/net/s3 :clan/net/simple-http-client :clan/net/websocket :clan/net/json-rpc.net
  :clan/utils/assert :clan/utils/base :clan/utils/basic-parsers :clan/utils/basic-printers
  :clan/utils/call-limiter :clan/utils/concurrency :clan/utils/config
  :clan/utils/date :clan/utils/debug :clan/utils/diceware
  :clan/utils/error :clan/utils/exit :clan/utils/ffi :clan/utils/files :clan/utils/filesystem
  :clan/utils/generator :clan/utils/hash
  :clan/utils/json :clan/utils/list :clan/utils/logger
  :clan/utils/maybe :clan/utils/memo :clan/utils/multicall :clan/utils/number :clan/utils/option
  :clan/utils/path :clan/utils/path-config :clan/utils/peekable-iterator
  :clan/utils/random :clan/utils/source
  :clan/utils/simple-rpc-client :clan/utils/stateful-avl-map :clan/utils/string
  :clan/utils/subprocess :clan/utils/temporary-files
  :clan/utils/vector :clan/utils/version :clan/utils/watch

  ;; POO
  ;;:clan/poo/poo (only-in :clan/poo/mop new) :clan/poo/type :clan/poo/io

  ;; Glow
  (for-template :glow/compiler/syntax-context)
  :glow/config/path :glow/config/version
  :glow/compiler/syntax-context
  :glow/compiler/passes :glow/compiler/multipass :glow/compiler/common
  :glow/compiler/alpha-convert/symbolnat :glow/compiler/alpha-convert/fresh
  :glow/compiler/alpha-convert/alpha-convert
  :glow/compiler/desugar/desugar
  :glow/compiler/typecheck/typecheck
  :glow/compiler/anf/anf
  :glow/compiler/participantify/participantify
  :crypto/keccak :crypto/secp256k1
  :glow/ethereum/hex :glow/ethereum/types

  ;; Testing Glow
  :glow/t/common
  ;:glow/compiler/t/multipass-test
  ;:glow/compiler/alpha-convert/t/alpha-convert-test
  ;:glow/compiler/anf/t/anf-test
)

;;(import :clan/poo/brace)

;; COPY THE LINE BELOW to files you try to debug
(import :clan/utils/debug)

;;(extern namespace: #f add-load-path) (add-load-path (glow-src))

;;(printf "Welcome, Glow hacker\n")

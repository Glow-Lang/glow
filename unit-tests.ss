#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :clan/testing)
(init-test-environment!)
(import
  :clan/path-config
  :clan/crypto/secp256k1
  :mukn/ethereum/types :mukn/ethereum/ethereum
  :mukn/glow/all-glow :mukn/glow/version :mukn/glow/t/common :mukn/glow/compiler/syntax-context
  :mukn/glow/cli/interaction :mukn/glow/cli/ethereum :mukn/glow/cli/contacts :mukn/glow/cli/identities
  :mukn/glow/runtime/glow-path)
(initialize-glow-path! [(source-path "dapps")])
;;(import :clan/debug)(DBG foo: (getenv "GERBIL_LOADPATH" #f) load-path)

#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :clan/testing)
(init-test-environment!)
(import
  (only-in :std/getopt rest-arguments)
  (only-in :clan/exit silent-exit)
  (only-in :clan/multicall define-entry-point set-default-entry-point!)
  :clan/path-config
  :clan/crypto/secp256k1
  :clan/ethereum/types :clan/ethereum/ethereum
  :mukn/glow/all-glow :mukn/glow/version :mukn/glow/t/common :mukn/glow/compiler/syntax-context
  :mukn/glow/cli/interaction :mukn/glow/cli/ethereum :mukn/glow/cli/contacts :mukn/glow/cli/identities
  :mukn/glow/runtime/glow-path)
(initialize-glow-path! [(source-path "dapps") (source-path "t/passdata")])
;;(import :clan/debug)(DBG foo: (getenv "GERBIL_LOADPATH" #f) load-path)

(define-entry-point (skip-tests . _)
  (help: "Return true without actually running tests"
   getopt: [(rest-arguments 'ignored help: "Ignored arguments")])
  (displayln "Tests disabled pending fixes and updates")
  (displayln "Use ./unit-tests unit-tests to run the tests")
  (displayln "Or get back to April 2022 version of glow, gerbil and all their dependencies")
  (displayln "commit 75d028222c5aa219f3499ce09fa34c854bca9153")
  (silent-exit))

(set-default-entry-point! 'skip-tests)

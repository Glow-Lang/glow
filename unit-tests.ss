#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :clan/testing)
(init-test-environment!)
(import
  :mukn/ethereum/types :mukn/ethereum/ethereum
  :mukn/glow/all-glow :mukn/glow/version :mukn/glow/t/common :mukn/glow/compiler/syntax-context)
;;(import :clan/debug)(DBG foo: (getenv "GERBIL_LOADPATH" #f) load-path)

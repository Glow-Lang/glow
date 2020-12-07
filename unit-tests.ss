#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :clan/t/test-support)
(init-test-environment!)
(import :mukn/glow/version :mukn/glow/t/common :mukn/glow/compiler/syntax-context :mukn/ethereum/signing :mukn/ethereum/types)

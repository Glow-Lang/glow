#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :utils/t/test-support)
(init-test-environment!)

gerbil/expander "config/path" :utils/ports)

(in-glow-src)
(import-module ':glow/t/unit-tests #t #t)
(import "compiler/syntax-context")
(def main (eval 'glow/t/unit-tests#main))

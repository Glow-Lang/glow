#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :gerbil/expander "config/path")
(current-directory (glow-src))
(add-load-path (glow-src))
(import-module ':glow/compiler/t/unit-tests #t #t)
(import "compiler/syntax-context")
(def main (eval 'glow/compiler/t/unit-tests#main))

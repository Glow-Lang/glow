#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import (only-in :clan/testing init-test-environment!))
(init-test-environment!) ;; sets up the path to load things in mukn/glow

(import
  (only-in :std/getopt rest-arguments)
  (only-in :clan/exit silent-exit)
  (only-in :clan/multicall define-entry-point set-default-entry-point!)
  (only-in :clan/path-config source-path)
  (only-in :mukn/glow/runtime/glow-path initialize-glow-path!)

  ;; Import for the REPL interactive environment?
  ;; :mukn/glow/all-glow

  ;; Import for the Glow compilation and evaluation environment:
  :mukn/glow/t/common :mukn/glow/compiler/syntax-context

  ;; Imported for the CLI command definitions:
  (only-in :mukn/glow/cli/interaction)
  (only-in :mukn/glow/cli/ethereum)
  (only-in :mukn/glow/cli/contacts)
  (only-in :mukn/glow/cli/identities)
  (only-in :mukn/glow/version)) ;; imported for version-defining side-effect

(initialize-glow-path! [(source-path "dapps") (source-path "t/passdata")])

#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the main build file for Glow. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")
;; Note that may you need to first:
;;   gxpkg install github.com/fare/gerbil-utils
;; See HACKING.md for details.

(import :std/misc/process :clan/building :clan/multicall)

(def (files) (cons "t/common.ss" (all-ss-files)))

(init-build-environment!
 name: "Glow"
 deps: '("clan" "clan/crypto" "clan/poo" "clan/persist" "mukn/ethereum")
 spec: files)

(define-entry-point (nix)
  "Build using nix-build"
  (create-version-file)
  (run-process ["nix-build"])
  (void))

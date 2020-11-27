#!/usr/bin/env gxi
;; This is the main build file for Glow. Invoke it using
;;     ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")
;; Note that may you need to first:
;;     gxpkg install github.com/fare/gerbil-ethereum
;; See HACKING.md for details.

(import :std/misc/process :clan/base :clan/building :clan/multicall)

(def (remove-file files file) ;; TODO: handle foo vs foo.ss ?
  (filter (match <>
            ((? (cut equal? <> file)) #f)
            ([gxc: (? (cut equal? <> file)) . _] #f)
            (_ #t)) files))

(def (add/options files file . options)
  (cons (cons* gxc: file options) (remove-file files file)))

(def (files)
  (!> (all-gerbil-modules)
      (cut cons* [exe: "main.ss" bin: "glow"] "t/common.ss" <>)
      (cut add/options <> "compiler/parse/expressions" "-cc-options" "-U___SINGLE_HOST")))

(init-build-environment!
 name: "Glow"
 ;; NB: missing versions for drewc/smug-gerbil and vyzo/libp2p
 deps: '("clan" "clan/crypto" "clan/poo" "clan/persist" "mukn/ethereum")
 spec: files)

(define-entry-point (nix)
  "Build using nix-build"
  (create-version-file)
  (run-process ["nix-build"])
  (void))

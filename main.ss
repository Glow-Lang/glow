#!/usr/bin/env gxi

(export #t)

(import
  :std/pregexp
  :clan/multicall :clan/versioning
  :clan/crypto/secp256k1
  :mukn/ethereum/ethereum :mukn/ethereum/types
  :mukn/glow/all-glow :mukn/glow/compiler/syntax-context :mukn/glow/t/common
  :mukn/glow/cli/contacts :mukn/glow/cli/interaction :mukn/glow/cli/identities)

(define-entry-point (git-version)
  (help: "Print the git version that Glow was compiled from" getopt: [])
  (def glow-vstring (cdr (assoc "Glow" software-layers)))
  (def v (match (pregexp-match "^.*-[0-9]+-g([0-9a-f]+)$" glow-vstring)
           ([_ v] v)
           (_ glow-vstring)))
  (displayln v))

(define-multicall-main)

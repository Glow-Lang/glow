#!/usr/bin/env gxi

(export #t)

(import
  :std/pregexp
  :clan/multicall :clan/versioning
  :clan/crypto/secp256k1
  :mukn/ethereum/ethereum :mukn/ethereum/types
  :mukn/glow/all-glow :mukn/glow/compiler/syntax-context :mukn/glow/t/common
  :mukn/glow/cli/contacts :mukn/glow/cli/interaction :mukn/glow/cli/identities
  :mukn/glow/runtime/glow-path)

(current-program "glow")
(initialize-glow-path! [])
(define-multicall-main)

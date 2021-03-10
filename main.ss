#!/usr/bin/env gxi

(export #t)

(import
  :std/pregexp
  :clan/multicall :clan/versioning
  :clan/crypto/secp256k1
  :mukn/ethereum/ethereum :mukn/ethereum/types
  :mukn/glow/all-glow :mukn/glow/compiler/syntax-context :mukn/glow/t/common
  :mukn/glow/cli/contacts :mukn/glow/cli/interaction :mukn/glow/cli/identities)

(current-program "glow")
(define-multicall-main)

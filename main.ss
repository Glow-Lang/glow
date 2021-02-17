#!/usr/bin/env gxi

(export #t)

(import
  :std/pregexp
  :clan/multicall :clan/versioning
  :mukn/glow/all-glow
  :mukn/glow/t/common :mukn/glow/compiler/syntax-context :mukn/ethereum/signing :mukn/ethereum/types
  :mukn/glow/runtime/cli
  )

(define-entry-point (git-version)
  "Print the git version that Glow was compiled from"
  (def glow-vstring (cdr (assoc "Glow" software-layers)))
  (def v (match (pregexp-match "^.*-[0-9]+-g([0-9a-f]+)$" glow-vstring)
           ([_ v] v)
           (_ glow-vstring)))
  (displayln v))

(def (main . args)
  (apply call-entry-point args))

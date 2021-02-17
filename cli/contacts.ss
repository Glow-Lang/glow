#!/usr/bin/env gxi
(export #t)

(import
  :gerbil/expander :gerbil/gambit/ports
  :std/format :std/generic :std/getopt :std/iter :std/misc/hash :std/misc/repr :std/misc/string :std/pregexp :std/sort :std/srfi/13 :std/sugar :std/text/json
  :clan/base :clan/cli :clan/exit :clan/json :clan/multicall :clan/path-config :clan/syntax
  :clan/poo/brace :clan/poo/debug :clan/poo/object
  :clan/persist/db :clan/persist/content-addressing :clan/versioning
  :mukn/ethereum/assets :mukn/ethereum/cli :mukn/ethereum/ethereum :mukn/ethereum/network-config
  :mukn/ethereum/signing :mukn/ethereum/types :mukn/ethereum/json-rpc :mukn/ethereum/known-addresses
  :mukn/glow/runtime/participant-runtime :mukn/glow/runtime/reify-contract-parameters :mukn/glow/runtime/configuration
  :mukn/glow/runtime/program :mukn/glow/runtime/terminal-codes
  (only-in :mukn/glow/compiler/alpha-convert/alpha-convert init-syms)
  :mukn/glow/compiler/passes :mukn/glow/compiler/multipass :mukn/glow/compiler/syntax-context)

(define-entry-point (add . arguments)
  "Add contact"
  (void))

(define-entry-point (remove . arguments)
  "Remove contact"
  (void))

(define-entry-point (list . arguments)
  "List contacts"
  (void))

(set-default-entry-point! list)
(def main call-entry-point)

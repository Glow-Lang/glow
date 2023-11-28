#!/usr/bin/env gxi

(export #t)

(import
  :mukn/glow/all-glow ;; required for the side-effect of importing everything that matters
  (only-in :std/cli/multicall call-entry-point current-program)
  (only-in :mukn/glow/runtime/glow-path initialize-glow-path!)
  :mukn/glow/compiler/syntax-context ;; make sure the syntax-context is here
  ;; other cli entry-points
  :mukn/glow/cli/contacts :mukn/glow/cli/interaction :mukn/glow/cli/identities
  :mukn/glow/contacts/server)

(current-program "glow")
(initialize-glow-path!)

(def (main . args)
  (gerbil-load-expander!) ;; NB: it will fail if called at the toplevel
  (apply call-entry-point args))

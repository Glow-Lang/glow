;; This module's purpose is to establish a common context
;; in which to parse and analyze source files for Glow.
;; See glow/compiler/common#read-sexp-file

(import
  (rename-in :gerbil/core (lambda λ))
  :gerbil/core
  :std/sugar
  ;;:clan/base
  :mukn/glow/runtime/context
  )

(export
  ;;@make-interaction
  ;;@ @interaction @publicly! @verifiably! interaction verifiably! publicly!
  λ assert! new
  (import: :gerbil/core))

;; This module's purpose is to establish a common context
;; in which to parse and analyze source files for Glow.
;; See glow/compiler/common#read-sexp-file

(import
  (rename-in :gerbil/core (lambda λ))
  :gerbil/core
  :std/sugar
  ;;:clan/utils/base
  )

(export
  ;;@ @interaction @publicly @verifiably interaction verifiably publicly
  λ assert!
  (import: :gerbil/core))

;;(defrule (dummies x ...) (begin (def (x) #f) ...))
;;(dummies @ @interaction @publicly @verifiably interaction verifiably publicly)

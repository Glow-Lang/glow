;; This module's purpose is to establish a common context
;; in which to parse and analyze source files for Glow.
;; See glow/compiler/common#read-sexp-file

(import
  (rename-in :gerbil/core (lambda λ))
  :gerbil/core)

(export
  λ
  (import: :gerbil/core))

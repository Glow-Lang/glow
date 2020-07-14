(export
  ;;@make-interaction
  ;;@ @interaction @publicly @verifiably interaction verifiably publicly
  λ assert! new
  (import: :gerbil/core))

(import
  (rename-in :gerbil/core (lambda λ))
  :gerbil/core
  :std/sugar
  (only-in :poo/mop new)
  ;;:utils/base
  )

(defrule (dummies x ...) (begin (def (x) #f) ...))
;;(dummies @make-interaction)
;;(dummies @ @interaction @make-interaction @publicly @verifiably interaction verifiably publicly)

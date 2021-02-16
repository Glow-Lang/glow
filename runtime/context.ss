(export
  ;;@make-interaction
  ;;@ @interaction @publicly! @verifiably! interaction verifiably! publicly!
  λ assert! new
  (import: :gerbil/core))

(import
  (rename-in :gerbil/core (lambda λ))
  :gerbil/core
  :std/sugar
  ;;:clan/base
  )

(defrule (dummies x ...) (begin (def (x) #f) ...))
(dummies new)
;;(dummies @make-interaction)
;;(dummies @ @interaction @make-interaction @publicly! @verifiably! interaction verifiably! publicly!)

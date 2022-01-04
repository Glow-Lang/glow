
{-# OPTIONS --cubical  #-}
module Glow.DApps.BoolGame where

open import Agda.Builtin.String
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List
open import Cubical.Data.Maybe
open import Cubical.Data.Bool

open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.AST


open GlowAST String

app : App 
app =
  𝓢-statement-with-attribute
    (attribute (identifier "interaction")
      (just (𝓔-Identifier (identifier "A") ∷ 𝓔-Identifier (identifier "B") ∷ [])))
      (𝓢-function-definition
        (identifier "interaction")
        []
        nothing
        (𝓔-body-expression
           (𝓢-expression-statement (𝓔-deposit-expression (identifier "A") (𝓔-Literal (numeric-literal 1)))  ∷
            𝓢-expression-statement (𝓔-deposit-expression (identifier "B") (𝓔-Literal (numeric-literal 1))) ∷
            𝓢-statement-with-attribute
               (attribute (identifier "A") nothing)
               (𝓢-value-definition (identifier "a") nothing
                 (𝓔-call-expression (𝓔-Identifier (identifier "input"))
                   (arguments (𝓔-Identifier (identifier "Bool") ∷ 𝓔-Literal (string-literal "Enter A's choice.") ∷ [])))
               ) ∷
            𝓢-publish-statement (identifier "A") ((identifier "a") ∷ []) ∷
            𝓢-statement-with-attribute
               (attribute (identifier "B") nothing)
               (𝓢-value-definition (identifier "b") nothing
                 (𝓔-call-expression (𝓔-Identifier (identifier "input"))
                   (arguments (𝓔-Identifier (identifier "Bool") ∷ 𝓔-Literal (string-literal "Enter B's choice.") ∷ [])))
               ) ∷
            𝓢-publish-statement (identifier "B") ((identifier "b") ∷ []) ∷
           []) (just
           (𝓔-if-expression
             (𝓔-binary-expression (𝓔-Identifier  (identifier "a")) (op "==") (𝓔-Identifier  (identifier "b")))
             (𝓔-body-expression
                (𝓢-expression-statement
                   (𝓔-withdraw-expression (identifier "A") (𝓔-Literal (numeric-literal 2))) ∷ [])
                nothing)
             (𝓔-body-expression
                 (𝓢-expression-statement
                   (𝓔-withdraw-expression (identifier "A") (𝓔-Literal (numeric-literal 2))) ∷ [])
                 nothing))))) ∷
  []

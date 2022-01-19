
{-# OPTIONS --cubical  #-}
module Glow.Simple.PropModeTransport where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod renaming (map to map-prod)
open import Cubical.Data.Sum renaming (elim to sum-elim ; map to map-sum)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.DecEqMore

open import Glow.Simple.AST



module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}} where

  
  open AST Identifier {{IsDiscrete-Identifier = IsDiscrete-Identifier}}

  pmtIdentifierWithType : ∀ {b₁ b₂} → IdentifierWithType b₁ → IdentifierWithType b₂
  pmtIdentifierWithType (name₁ ∶ type₁) = name₁ ∶ type₁



-- AST.interactionHead ( x .participants) (map-List pmtIdentifierWithType (x .parameters)) {{!x .uniqueParams!}}
  {-# TERMINATING #-}
  pmtIh : ∀ {b₁ b₂} → InteractionHead b₁ → InteractionHead b₂
  pmtIh {b₂ = b₂} (AST.interactionHead participants₁ []) = AST.interactionHead participants₁ [] {PropMode.fromWitness' b₂ _ }
  pmtIh {b₁} {b₂} (AST.interactionHead participants₁ (name₁ ∶ type₁ ∷ parameters₁) {yy}) = 
     let z = (pmtIh (AST.interactionHead participants₁ (parameters₁) {PropMode.fromWitness' b₁ (proj₂ (PropMode.toWitness' b₁ yy)) }))
     in  record z { parameters = (name₁ ∶ type₁) ∷ (z .parameters)
                  ; uniqueParams = PropMode.fromWitness' b₂ ( {! (proj₁ (PropMode.toWitness' b₁ yy))!} , PropMode.toWitness' b₂ (z .uniqueParams))
                  }

  

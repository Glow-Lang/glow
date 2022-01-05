
{-# OPTIONS --cubical  #-}
module Glow.ScopeTest where

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
open import Cubical.Data.Bool

open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

data ScopeTest : Type₀

IsInScope : ScopeTest → ℕ → Type₀

infixr 10 _↑_
infixr 10 _↓_


data ScopeTest where
 ∙ :  ScopeTest
 _↑_ :  ℕ → ScopeTest → ScopeTest
 _↓_ :  (s : ℕ) → (st : ScopeTest) → {inScope : IsInScope st s} → ScopeTest

IsInScope ∙ x₁ = ⟨ ⊥ ⟩ 
IsInScope (x ↑ x₂) x₁ with discreteℕ x₁ x
... | yes p = ⟨ ⊤ ⟩ 
... | no ¬p = IsInScope x₂ x₁
IsInScope (s ↓ x) x₁ = IsInScope x x₁
 


test : ScopeTest
test =  2 ↓ 4 ↓ 4 ↑ 2 ↑ ∙  

-- test-Bad : ScopeTest
-- test-Bad =  3 ↓ 4 ↓ 4 ↑ 2 ↑ ∙  

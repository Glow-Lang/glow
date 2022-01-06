
{-# OPTIONS --cubical  #-}
module Glow.Identifier where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List


open import Cubical.Data.Maybe renaming (rec to recMaybe ;  nothing to ◦)
open import Cubical.Data.Bool renaming (if_then_else_ to if_then_else'_)

open import Cubical.Data.Nat.Order.Recursive
open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)


-- data F (n : ℕ) : Type₀



-- data F n where
--   fzero' : (0 < n) → F n
--   fsuc' : G (predℕ n) → F n

Disc→Ty : ∀ {ℓ} → ∀ {A : Type ℓ} → Dec A → Type₀
Disc→Ty (yes p) = ⟨ ⊤ ⟩
Disc→Ty (no ¬p) = ⟨ ⊥ ⟩

=ℕTy : ℕ → ℕ → Type₀
=ℕTy x x₁ = Disc→Ty (discreteℕ x x₁)

data Linked {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) (c : C) : Type₀

foldLinked : {C : Type₀} {A : C → Type₀} {fld : ∀ c → A c → C} {c : C}
                → Linked fld c → C

data Linked {C} {A} fld c  where
  L[] : Linked fld c
  _L∷_ : (x : Linked fld c) → A (foldLinked x) → Linked fld c

foldLinked {c = c} L[] = c
foldLinked {fld = fld} (x L∷ x₁) = fld (foldLinked x) x₁


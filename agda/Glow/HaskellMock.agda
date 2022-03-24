{-# OPTIONS --cubical  #-}
module Glow.HaskellMock where


open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Fin
open import Cubical.Data.Int
open import Cubical.Data.Prod renaming (map to map-prod)
open import Cubical.Data.Sum renaming (elim to sum-elim ; map to map-sum)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool renaming (Bool to 𝟚 ; _≟_ to _≟B_)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )



record MapInterface ℓ : Type (ℓ-suc ℓ) where
  field
    Map : Type ℓ → Type ℓ → Type ℓ
    lookup : ∀ {A B} → Map A B → A → B
    adjust : ∀ {a k} → (a -> a) -> k -> Map k a -> Map k a
    -- update : ∀ {A B} → Map A B → A → B




postulate postulatedMapImplementation : MapInterface ℓ-zero  


module M = MapInterface postulatedMapImplementation

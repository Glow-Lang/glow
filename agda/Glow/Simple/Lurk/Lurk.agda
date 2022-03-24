{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.Lurk where




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
open import Cubical.Data.Bool hiding (if_then_else_) renaming (Bool to 𝟚 ; _≟_ to _≟B_)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )




module Safe where

  postulate Program : Type₀

  postulate Arguments : Program → Type₀

  postulate Verifier : Program → Type₀
  postulate Prover : Program → Type₀

  postulate Proof : {p : Program} → Arguments p → Type₀

  postulate verify : {p : Program}  → Verifier p → Arguments p → 𝟚



module Unsafe where

-- TODO : rename arguments to input
  postulate Program : Type₀

  postulate Arguments : Type₀

  postulate Verifier : Type₀
  postulate Prover : Type₀

  postulate Proof : Type₀

  postulate verify : Verifier → Arguments → Proof → 𝟚


  

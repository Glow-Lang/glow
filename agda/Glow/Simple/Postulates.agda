{-# OPTIONS --cubical  #-}
module Glow.Simple.Postulates where

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat 
open import Cubical.Data.Bool 


data Dig : Type₀ where
  dig : ℕ → Dig 

postulate isSet-Dig : isSet Dig
-- isSet-Dig (dig x) (dig x₂) x₁ y₁ i i₁ = {!dig ?!}

data Sig : Type₀ where
  sigPrim : ℕ → Sig 

postulate isSet-Sig : isSet Sig

postulate digestPrim : {A : Type₀} → A → Dig

postulate digestEqTestPrim : Dig → Dig → Bool


postulate signPrim : {A : Type₀} → A → Dig → Sig

postulate randomUInt256PrimUnEvaluated : ℕ

postulate randomUInt256PrimEvaluated : ℕ → ℕ


postulate ^^^Prim : ℕ → ℕ → ℕ

postulate &&&Prim : ℕ → ℕ → ℕ

postulate ==Digest : Dig → Dig → Bool

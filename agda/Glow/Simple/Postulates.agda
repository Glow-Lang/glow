{-# OPTIONS --cubical  #-}
module Glow.Simple.Postulates where

open import Cubical.Foundations.Everything 


postulate Dig : Type₀

postulate Sig : Type₀

postulate digestPrim : {A : Type₀} → A → Dig

postulate signPrim : {A : Type₀} → A → Dig → Sig

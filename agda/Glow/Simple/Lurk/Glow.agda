{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.Glow where




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

import Glow.Simple.Lurk.Lurk as Lurk

open import Glow.Simple.AST


module Abstract where

  module LA = Lurk.Abstract 

  record Precompiled : Type₀ where
    field     
      maxStateId : ℕ
      participants : ℕ
      reachable : Fin maxStateId → LA.Program 






module Concrete where

  Map : Type₀ → Type₀ → Type₀
  Map a b = a → b 

  module LC = Lurk.Concrete

  StateId = ℕ
  ParticipantId = ℕ

  data Action : Type₀ where
    depositA : ℕ → Action
    withdrawA : ℕ → Action
    publishA : GlowValue → Action 


  record OnChainState : Type₀ where
    field
      balances : Map ParticipantId ℕ
      currentSate : StateId
      published : List GlowValue
      

  record CompiledContract : Type₀ where
    field
      stateTransitionsVerifier : LC.Verifier

  record InteractionCall : Type₀ where
    constructor iC
    field
      caller : ParticipantId
      desiredState : StateId
      action : Action

  open InteractionCall
  open OnChainState


  advanceState : OnChainState → InteractionCall → OnChainState
  advanceState s ic@(iC c dS a) = h a
    where
      h : Action → _
      h (depositA x) = {!!}
      h (withdrawA x) = {!!}
      h (publishA x) = record s { published =  x ∷ s .published  }


  record UniversalHostContract : Type₀ where
    field
      makeArguments : OnChainState → InteractionCall → LC.Arguments      

    record Deployment : Type₀ where
      field
        compiledContract : CompiledContract

      open CompiledContract compiledContract


      interact : OnChainState → (InteractionCall × LC.Proof) → OnChainState
      interact s₀ (ic , proof) =
        if (LC.verify stateTransitionsVerifier (makeArguments s₀ ic) proof)   
        then (advanceState s₀ ic)
        else s₀



 
  








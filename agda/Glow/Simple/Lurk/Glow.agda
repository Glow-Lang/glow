{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.Glow where




open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Fin
open import Cubical.Data.Int renaming (_+_ to _+ℤ_)
open import Cubical.Data.Prod renaming (map to map-prod)
open import Cubical.Data.Sum renaming (elim to sum-elim ; map to map-sum)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool renaming (Bool to 𝟚 ; _≟_ to _≟B_)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )

open import Glow.HaskellMock

import Glow.Simple.Lurk.Lurk as Lurk

open import Glow.Simple.AST


module Safe where

  module L = Lurk.Safe 

  data Action : Type₀ where
    depositA : ℕ → Action
    withdrawA : ℕ → Action
    publishA : GlowValue → Action 

  module _ (statesNumber participantsNumber : ℕ) where 

    StateId = Fin statesNumber
    ParticipantId = Fin participantsNumber

    

    record OnChainState : Type₀ where
      field
        balances : ParticipantId → ℕ
        currentSate : StateId
        published : List GlowValue


    open OnChainState


    record CompiledContract : Type₀ where
      field
        stateTransitionsProgram : L.Program
        stateTransitionsVerifier : L.Verifier stateTransitionsProgram

      record InteractionCall : Type₀ where
        pattern
        constructor iC
        field
          caller : ParticipantId
          desiredState : StateId
          action : Action

      open InteractionCall public
      
     
      advanceState : OnChainState → InteractionCall → OnChainState
      advanceState s ic@(iC c dS a) = h a
        where
          h : Action → _
          h (depositA x) = record s { balances =  {!!} }
          h (withdrawA x) = record s { balances = {!!} }
          h (publishA x) = record s { published =  x ∷ s .published  }


    record UniversalHostContract : Type₀ where
      field
        makeArguments : ∀ {cc} → OnChainState → CompiledContract.InteractionCall cc → L.Arguments (cc .CompiledContract.stateTransitionsProgram)      

      record Deployment : Type₀ where
        field
          compiledContract : CompiledContract

        open CompiledContract compiledContract

        record SignedInteractionCall (onChainState : OnChainState) : Type₀ where
          pattern
          constructor siC
          field
            payload : InteractionCall
            proof : L.Proof stateTransitionsProgram (makeArguments onChainState payload ) 
            

module Unsafe where

  module L = Lurk.Unsafe

  StateId = ℕ
  ParticipantId = ℕ

  data Action : Type₀ where
    depositA : ℕ → Action
    withdrawA : ℕ → Action
    publishA : GlowValue → Action


  record OnChainState : Type₀ where
    field
      balances : M.Map ParticipantId ℕ
      currentSate : StateId
      published : List GlowValue
      

  record CompiledContract : Type₀ where
    field
      stateTransitionsVerifier : L.Verifier

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
      h (depositA x) = record s { balances =  M.adjust (_+ x) c (s .balances) }
      h (withdrawA x) = record s { balances = M.adjust (_∸ x) c (s .balances) }
      h (publishA x) = record s { published =  x ∷ s .published  }
    

  record UniversalHostContract : Type₀ where
    field
      makeArguments : OnChainState → InteractionCall → L.Arguments      

    record Deployment : Type₀ where
      field
        compiledContract : CompiledContract

      open CompiledContract compiledContract


      interact : OnChainState → (InteractionCall × L.Proof) → OnChainState
      interact s₀ (ic , proof) =
        if (L.verify stateTransitionsVerifier (makeArguments s₀ ic) proof)   
        then (advanceState s₀ ic)
        else s₀



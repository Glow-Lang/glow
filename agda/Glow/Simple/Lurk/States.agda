{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.States where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Fin
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool hiding (if_then_else_)  renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.Simple.AST

open import Glow.DecEqMore

open import Glow.Simple.ContextMore

open import Glow.Simple.VarSubst

open import Glow.Simple.ParamsSubst

-- open import Glow.Simple.Monad


open import Cubical.HITs.Interval

open import Glow.ListDecProps



module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
            {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where


  module TraceNice {ptpsIds : List (Identifier)} where

    ptps : List (Identifier × ParticipantModality)
    ptps = map-List (_, dishonest) ptpsIds
    
    module _ {uniquePtps : _} where
    
      open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps [] {_} {uniquePtps}) 


      open SubstAll {Identifier} {builtIns = builtIns} {ptps = ptps}
      open SubstOne {Identifier} {builtIns = builtIns} {ptps = ptps}


      record TargetState (numberOfStates : ℕ) (Γ : Context) : Type₀ where
        field
          targetStateId : Fin numberOfStates
          guardExpr : Expr Γ Bool --(stateContext sId) Bool
          action : ?


      

      record TranslationReady : Type₀ where
        field
          numberOfStates : ℕ
        StateId = Fin numberOfStates

        field
          stateContextVars : StateId → List (GType × Identifier)
        stateContext : StateId → Context
        stateContext x = con (map-List (λ x₁ → AST.ice nothing (proj₂ x₁) (proj₁ x₁)) (stateContextVars x)) nothing
        
        field
          reachableStates : (sId : StateId) → (Γ : Rec (stateContext sId))
               → List (TargetState numberOfStates (stateContext sId))

      emptyTranslationReady : TranslationReady
      TranslationReady.numberOfStates emptyTranslationReady = zero
      TranslationReady.stateContextVars emptyTranslationReady = empty-rec ∘ ¬Fin0
      TranslationReady.reachableStates emptyTranslationReady = empty-rec ∘ ¬Fin0

      -- TranslationReady : ℕ → ℕ → Σ (Expr (con [] nothing) Bool) (λ e → IsEmpty ⟨ IsPureE e ⟩ ) 
      -- TranslationReady = {!!}

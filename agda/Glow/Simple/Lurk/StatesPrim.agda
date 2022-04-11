{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.StatesPrim where

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

      data Action : Type₀ where
        withdrawA : Action
        depositA : Action
        publishA : Action

      record StateInfo  : Type₀ where
        field
          stateContextVars : List (GType × Identifier)
          action : Action
          caller : DishonestParticipantId
          

          -- targetStateId : Fin numberOfStates
          -- guardExpr : Expr Γ Bool --(stateContext sId) Bool
      open StateInfo
      

      record TranslationReady : Type₀ where
        field
          numberOfStates : ℕ
        StateId = Fin (numberOfStates)

        field
          stateInfo : StateId → StateInfo
        stateContextVarsAll : StateId → List (GType × Identifier)
        stateContextVarsAll = (stateContextVars ∘ stateInfo)
        -- ... | inl x₁ = []
        -- ... | inr x = let si = stateInfo x
        --               in (stateContextVars si)

        stateContext : StateId → Context
        stateContext x = con (map-List (λ x₁ → AST.ice nothing (proj₂ x₁) (proj₁ x₁)) (stateContextVarsAll x)) nothing
        
          
        field
          reachableStates : (sId : StateId)  -- → (Γ : Rec (stateContext sId))
                → List (StateId × Expr (stateContext sId) Bool)
  

        OpenStates : Type₀
        OpenStates = List (StateId)

      open TranslationReady


      emptyTranslationReady : TranslationReady
      numberOfStates emptyTranslationReady = zero
      stateInfo emptyTranslationReady = empty-rec ∘ ¬Fin0
      reachableStates emptyTranslationReady _ = []
      
      TRS = Σ TranslationReady OpenStates

      compTRS : List TRS → TRS → TRS
      compTRS hs t = {!!}

      postulate never : Empty 


      {-# TERMINATING #-}
      foldTR : Statements (con [] nothing) → TranslationReady

      foldTRB : ∀ {Τ} → (s : Stmnt (con [] nothing))
                → (bd : Body (bindingMechanics' _ s ) Τ)
                → IsEmpty ⟨ IsPureE (body (AST.bodyR (s ∷L (stmnts bd)) (expr bd))) ⟩ 
                → TRS

      foldTRE : ∀ {Τ} → (e : Expr (con [] nothing) Τ)
                → IsEmpty ⟨ IsPureE e ⟩ 
                → TRS


      foldTR []L = emptyTranslationReady
      foldTR (h ∷L x) with proj₁ (snd (IsPureS h))
      ... | no ¬p = fst (foldTRB h (AST.bodyR x (lit tt)) (¬p ∘ proj₁ ∘ proj₁))
      ... | yes p with h
      ... | AST.bindingS (AST.BS-let ce x₁) =
                  let y = (substOneStmnts (inl (evalPureExpr x₁ p)) (mkStatements* x))
                  in foldTR y
      ... | AST.nonBindingS x₁ = foldTR x


      foldTRB {Τ} (AST.bindingS (AST.BS-let ce x₁)) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureE x₁)))
      ... | yes p =
         let v = evalPureExpr x₁ p
         in foldTRE ((substOneExpr (inl v) (body bo))) λ _ → never
      ... | no ¬p with (proj₁ (snd (IsPureE (body bo))))
      ... | yes p = {!!}
      ... | no ¬p₁ = {!!}

      foldTRB (AST.bindingS (AST.BS-publish! p (AST.psof name {()}))) (AST.bodyR stmnts₁ expr₁) x₁

      foldTRB s@(AST.nonBindingS y) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureS s))) | (proj₁ (snd (IsPureE (body bo)))) 
      ... | yes p | _ = {!!}
      ... | no ¬p | yes p = {!!}
      ... | no ¬p | no ¬p₁ = {!!}

  
      foldTRE e x = {!!}



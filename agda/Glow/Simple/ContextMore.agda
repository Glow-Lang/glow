
{-# OPTIONS --cubical  #-}
module Glow.Simple.ContextMore where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.DecEqMore

open import Glow.Simple.AST

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}} where


  -- open AST Identifier

  module _ {ptps : List Identifier} where
  
    open AST.InteractionHead (AST.interactionHead ptps [])

    {-# TERMINATING #-}
    Subst' : List ContextEntry → Type₀
    Subst' [] = Empty
    Subst' (x ∷ x₁) = AType x ⊎ Subst' x₁ 


    Subst : Context → Type₀
    Subst = Subst' ∘ entries 

    remSubst' : ∀ l → Subst' l → List ContextEntry
    remSubst' (x₁ ∷ l) (inl x) = (fst (FilterOut (((ce-name x₁) ≡_) ∘ ce-name) l))
    remSubst' (x₁ ∷ l) (inr x) = x₁ ∷ remSubst' l x

    remSubst : ∀ Γ → Subst Γ → Context
    remSubst Γ x = record Γ { entries = remSubst' (Γ .entries) x }



    {-# TERMINATING #-}
    Rec' : List ContextEntry → Type₀
    Rec' [] = Unit
    Rec' (x ∷ x₁) = AType x × Rec' (fst (FilterOut (((ce-name x) ≡_) ∘ ce-name) x₁))

    Rec : Context → Type₀
    Rec = Rec' ∘ entries 

    Statement* : ∀ { B : Context → Type₀ } → Σ Context B → Type₀
    Statement* = Stmnt ∘ fst

    -- data IOs : Type₀ where
    --   inputBy : IOs
    --   timedOutInput

    -- toConsensusCodeStmnt : ∀ {s} → Stmnt (con [] s) → Stmnt emptyContext
    -- toConsensusCodeStmnt {nothing} x = x
    -- toConsensusCodeStmnt {just x₁} (AST.bindingS x) = {!!}
    -- toConsensusCodeStmnt {just x₁} (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! x))) = (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! {!!})))
    -- toConsensusCodeStmnt {just x₁} (AST.nonBindingS (AST.exprNBS x)) = {!!}


    publish-substlemma : {c : Context} (r : Subst c)
                       (p : AST.ParticipantId (AST.interactionHead ptps parameters))
                       (x : AST.PrivateSymbolOf c p)
                       {z : True (snd (AST.IsConsensus c))} →
                     Subst (bindingMechanics' c (bindingS (BS-publish! p x {z})))
    publish-substlemma {AST.con (AST.ice nothing name₁ type ∷ entries₁) nothing} r p (AST.psof name {q}) with toWitness
    publish-substlemma {AST.con (AST.ice (just x) name₁ type ∷ entries₁) nothing} r p (AST.psof name {q}) = {!!}

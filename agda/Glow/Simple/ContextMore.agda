
{-# OPTIONS --cubical  #-}
module Glow.Simple.ContextMore where

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
open import Cubical.Data.Bool renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.DecEqMore

open import Glow.Simple.AST



module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}} where

  prop-mode = true
  
  -- open AST Identifier prop-mode

  open PropMode prop-mode 

  -- open AST Identifier

  module _ {ptps : List Identifier} where
  
    open AST.InteractionHead {prop-mode = prop-mode} (AST.interactionHead ptps []) 

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

    map-ExistingFirstBy-lemma : {cs : List ContextEntry}
                       (B : ContextEntry → Type₀) (B' : ContextEntry → Type₀)
                       
                       (z : ExistFirstBy B WitchIsAlso B' cs) →
                       (f : ContextEntry → Scope)
                       (r : Subst' cs) → Subst' (map-ExistingFirstBy B WitchIsAlso B' cs z λ x x₁ x₂ → record x { scope = f x })
    map-ExistingFirstBy-lemma {x ∷ cs} B B' (inl x₁) f = idfun _
    map-ExistingFirstBy-lemma {x ∷ cs} B B' (inr x₁) f = map-sum (idfun _) (map-ExistingFirstBy-lemma {cs} B B' (proj₂ x₁) f)


    publish-substlemma : {c : Context} (r : Subst c)
                       (p : AST.ParticipantId (AST.interactionHead ptps parameters))
                       (x : AST.PrivateSymbolOf c p)
                       (z : ⟨ AST.IsConsensus c ⟩) →
                     Subst (bindingMechanics' c (bindingS (BS-publish! p x {z})))
    publish-substlemma r p x _ = map-ExistingFirstBy-lemma (λ x₁ → psof-name _ x ≡ AST.name x₁) (λ y →
                                                                                                  recMaybe Empty (λ p' → AST.pId-name _ _ p ≡ AST.pId-name _ _ p')
                                                                                                  (AST.scope y))  (psof-proof _ x) (λ _ → nothing) r


    ExistFirstBy-WitchIsAlso-remSubs-lemm : {nm : Identifier} {p : ParticipantId} (l : List ContextEntry) → (r : Subst' l)  →
                                                      ExistFirstBy ((nm ≡_) ∘ ce-name) WitchIsAlso
                                                        ((λ y → recMaybe Empty (λ p' → (AST.pId-name _ _ p) ≡ (AST.pId-name _ _ p')) (ce-scope y))) l →
                                                      ((ExistFirstBy ((nm ≡_) ∘ ce-name) WitchIsAlso
                                                         ((λ y → recMaybe Empty (λ p' → (AST.pId-name _ _ p) ≡ (AST.pId-name _ _ p')) (ce-scope y)))
                                                         (remSubst' l r)) ⊎ Unit)
    ExistFirstBy-WitchIsAlso-remSubs-lemm (x₁ ∷ l) (inl x) (inl x₂) = inr tt
    ExistFirstBy-WitchIsAlso-remSubs-lemm (x₁ ∷ l) (inl x) (inr x₂) = inl (ExistFirstBy-WitchIsAlso-FilterOut-lemma _ (λ a x₃ y → proj₁ x₂ (x₃ ∙ sym y)) (proj₂ x₂))
    ExistFirstBy-WitchIsAlso-remSubs-lemm (x₁ ∷ l) (inr x) (inl x₂) = inl (inl x₂)
    ExistFirstBy-WitchIsAlso-remSubs-lemm {nm} {p} (x₁ ∷ l) (inr x) (inr x₂) = map-sum
          (inr ∘ (proj₁ x₂ ,_))
          (λ _ → tt)
          (ExistFirstBy-WitchIsAlso-remSubs-lemm {nm} {p} l x (proj₂ x₂))

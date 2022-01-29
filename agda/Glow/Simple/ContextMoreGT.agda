
{-# OPTIONS --cubical  #-}
module Glow.Simple.ContextMoreGT where

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

open import Cubical.Relation.Nullary renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.DecEqMore

open import Glow.Simple.AST

open import Cubical.HITs.Interval


module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
            {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where

  -- prop-mode = one
  
  -- open AST Identifier prop-mode

  open PropMode one 

  -- open AST Identifier

  module _ {ptps : List Identifier} where
  
    -- module AST* = AST Identifier builtIns one

    open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps []) 

     
    Subst' : List ContextEntry → Type₀
    Subst' [] = Empty
    Subst' (x ∷ x₁) = AType x ⊎ Subst' x₁ 


    Subst : Context → Type₀
    Subst = Subst' ∘ entries 

    remSubst' : ∀ l → Subst' l → List ContextEntry
    remSubst' (x₁ ∷ l) (inl x) = (fst (FilterOut (((ce-name x₁) ≡_) ∘ ce-name) l))
    remSubst' (x₁ ∷ l) (inr x) = x₁ ∷ remSubst' l x

    -- remSubst : ∀ Γ → Subst Γ → Context
    -- remSubst Γ x = record Γ { entries = remSubst' (Γ .entries) x }



    -- {-# TERMINATING #-}
    -- Rec' : List ContextEntry → Type₀
    -- Rec' [] = Unit
    -- Rec' (x ∷ x₁) = AType x × Rec' (fst (FilterOut (((ce-name x) ≡_) ∘ ce-name) x₁))

    -- Rec : Context → Type₀
    -- Rec = Rec' ∘ entries 

    -- Statement* : ∀ { B : Context → Type₀ } → Σ Context B → Type₀
    -- Statement* = Stmnt ∘ fst

    -- -- data IOs : Type₀ where
    -- --   inputBy : IOs
    -- --   timedOutInput

    -- -- toConsensusCodeStmnt : ∀ {s} → Stmnt (con [] s) → Stmnt emptyContext
    -- -- toConsensusCodeStmnt {nothing} x = x
    -- -- toConsensusCodeStmnt {just x₁} (AST.bindingS x) = {!!}
    -- -- toConsensusCodeStmnt {just x₁} (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! x))) = (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! {!!})))
    -- -- toConsensusCodeStmnt {just x₁} (AST.nonBindingS (AST.exprNBS x)) = {!!}

    -- map-ExistingFirstBy-lemma : {cs : List ContextEntry}
    --                    (B : ContextEntry → Type₀) (B' : ContextEntry → Type₀)
                       
    --                    (z : ExistFirstBy B WitchIsAlso B' cs) →
    --                    (f : ContextEntry → Scope)
    --                    (r : Subst' cs) → Subst' (map-ExistingFirstBy B WitchIsAlso B' cs z λ x x₁ x₂ → record x { scope = f x })
    -- map-ExistingFirstBy-lemma {x ∷ cs} B B' (inl x₁) f = idfun _
    -- map-ExistingFirstBy-lemma {x ∷ cs} B B' (inr x₁) f = map-sum (idfun _) (map-ExistingFirstBy-lemma {cs} B B' (proj₂ x₁) f)


    -- publish-substlemma : {c : Context} (r : Subst c)
    --                    (p : AST.ParticipantId (AST.interactionHead ptps parameters))
    --                    (x : AST.PrivateSymbolOf c p)
    --                    (z : ⟨ AST.IsConsensus c ⟩) →
    --                  Subst (bindingMechanics' c (bindingS (BS-publish! p x {z})))
    -- publish-substlemma r p x _ = map-ExistingFirstBy-lemma (λ x₁ → psof-name _ x ≡ AST.name x₁) (λ y →
    --                                                                                               recMaybe Empty (λ p' → AST.pId-name _ _ _ p ≡ AST.pId-name _ _ _ p')
    --                                                                                               (AST.scope y))  (psof-proof _ x) (λ _ → nothing) r


    -- SubstMatchEntry : ∀ {ℓ} (B' : ContextEntry → Type ℓ) → (l : List ContextEntry) → (nm : Identifier)
    --                  → (r : Subst' l)
    --                  → ExistFirstBy ((nm ≡_) ∘ ce-name)
    --                      WitchIsAlso B' l
    --                        → Type₀
    -- SubstMatchEntry B' (x₁ ∷ l) nm (inl x) (inl x₂) = Unit
    -- SubstMatchEntry B' (x₁ ∷ l) nm (inl x) (inr x₂) = Empty
    -- SubstMatchEntry B' (x₁ ∷ l) nm (inr x) (inl x₂) = Empty
    -- SubstMatchEntry B' (x₁ ∷ l) nm (inr x) (inr x₂) = SubstMatchEntry B' (l) nm (x) (proj₂ x₂)

    -- SubstNotMatchEntry : ∀ {ℓ} (B' : ContextEntry → Type ℓ) → (l : List ContextEntry) → (nm : Identifier)
    --                  → (r : Subst' l)
    --                  → ExistFirstBy ((nm ≡_) ∘ ce-name)
    --                      WitchIsAlso B' l
    --                        → Type₀
    -- SubstNotMatchEntry B' (x₁ ∷ l) nm (inl x) (inl x₂) = Empty
    -- SubstNotMatchEntry B' (x₁ ∷ l) nm (inl x) (inr x₂) = Unit
    -- SubstNotMatchEntry B' (x₁ ∷ l) nm (inr x) (inl x₂) = Unit
    -- SubstNotMatchEntry B' (x₁ ∷ l) nm (inr x) (inr x₂) = SubstNotMatchEntry B' (l) nm (x) (proj₂ x₂)


    -- SubstMatchEntry? : ∀ {ℓ} (B' : ContextEntry → Type ℓ) → (l : List ContextEntry) → (nm : Identifier)
    --                  → (r : Subst' l)
    --                  → (y : ExistFirstBy ((nm ≡_) ∘ ce-name)
    --                      WitchIsAlso B' l)
    --                        → SubstNotMatchEntry _ _ _ r y ⊎ SubstMatchEntry _ _ _ r y
    -- SubstMatchEntry? B' (x ∷ l) nm (inl x₁) (inl x₂) = inr tt
    -- SubstMatchEntry? B' (x ∷ l) nm (inl x₁) (inr x₂) = inl tt
    -- SubstMatchEntry? B' (x ∷ l) nm (inr x₁) (inl x₂) = inl tt
    -- SubstMatchEntry? B' (x ∷ l) nm (inr x₁) (inr x₂) = SubstMatchEntry? B' l nm x₁ (proj₂ x₂) 

    -- SubstMatch-Extract : ∀ {ℓ} (B' : ContextEntry → Type ℓ) → (l : List ContextEntry) → (nm : Identifier)
    --                  → (r : Subst' l)
    --                  → (y : ExistFirstBy ((nm ≡_) ∘ ce-name)
    --                      WitchIsAlso B' l)
    --                        → SubstMatchEntry _ _ _ r y → Σ _ GTypeAgdaRep
    -- SubstMatch-Extract B' (AST.ice scope name type ∷ l) nm (inl x₂) (inl x₃) x = type , x₂
    -- SubstMatch-Extract B' (x₁ ∷ l) nm (inr x₂) (inr x₃) x = SubstMatch-Extract B' l nm x₂ (proj₂ x₃) x 

    -- ExistFirstBy-WitchIsAlso-remSubs-lemm : {nm : Identifier} {p : ParticipantId} (l : List ContextEntry) → (r : Subst' l)  →
    --                                                   (z : ExistFirstBy ((nm ≡_) ∘ ce-name) WitchIsAlso
    --                                                     ((λ y → recMaybe Empty (λ p' → (AST.pId-name _ _ _ p) ≡ (AST.pId-name _ _ _ p')) (ce-scope y))) l) →
                                                        
    --                                                   ((SubstNotMatchEntry _ _ _ r z × ExistFirstBy ((nm ≡_) ∘ ce-name) WitchIsAlso
    --                                                      ((λ y → recMaybe Empty (λ p' → (AST.pId-name _ _ _ p) ≡ (AST.pId-name _ _ _ p')) (ce-scope y)))
    --                                                      (remSubst' l r))
    --                                                    ⊎ (SubstMatchEntry _ _ _ r z  × IsEmpty (ExistMemberAs ((nm ≡_) ∘ ce-name) (remSubst' l r))))
    -- ExistFirstBy-WitchIsAlso-remSubs-lemm (x₁ ∷ l) (inl x) (inl x₂) =  
    --     inr (_ , ((snd (FilterOut (((ce-name x₁) ≡_) ∘ ce-name) l))
    --      ∘ subst (λ z → ExistMemberAs (λ x₃ → z ≡ AST.name x₃)
    --                      (fst (FilterOut (λ x₃ → AST.name x₁ ≡ AST.name x₃) l))) (proj₁ x₂)))
    -- ExistFirstBy-WitchIsAlso-remSubs-lemm (x₁ ∷ l) (inl x) (inr x₂) =
    --      inl (_ , (ExistFirstBy-WitchIsAlso-FilterOut-lemma _ (λ a x₃ y → proj₁ x₂ (x₃ ∙ sym y)) (proj₂ x₂)))
    -- ExistFirstBy-WitchIsAlso-remSubs-lemm (x₁ ∷ l) (inr x) (inl x₂) = inl (tt , (inl x₂))
    -- ExistFirstBy-WitchIsAlso-remSubs-lemm {nm} {p} (x₁ ∷ l) (inr x) (inr x₂) = map-sum
    --       (map-prod (idfun _)  (inr ∘ (proj₁ x₂ ,_)))
    --       (map-prod (idfun _) λ b → sum-elim (proj₁ x₂) (b ∘ proj₂))
    --        (ExistFirstBy-WitchIsAlso-remSubs-lemm {nm} {p} l x (proj₂ x₂))



    -- map-ExistingFirstBy-lemma2 : {cs : List ContextEntry} {nm : Identifier} 
    --                    (B' : ContextEntry → Type₀)
    --                    (z : ExistFirstBy ((nm ≡_) ∘ ce-name) WitchIsAlso B' cs) →
    --                    (f : ContextEntry → Scope)
    --                    (r : Subst' cs) →
    --                    (SubstMatchEntry _ _ _ r z) → 
    --                    remSubst' _ (map-ExistingFirstBy-lemma ((nm ≡_) ∘ ce-name) B' z f r) ≡ remSubst' cs r
    -- map-ExistingFirstBy-lemma2 {x₁ ∷ cs} B' (inl x) f (inl x₂) (x₃) = refl
    -- map-ExistingFirstBy-lemma2 {x₁ ∷ cs} B' (inr x) f (inr x₂) (x₃) = cong (x₁ ∷_) (map-ExistingFirstBy-lemma2 {cs} _ _ _ _ x₃)


    -- -- TODO : turn into more general version, with assumption of propositionality of predicates
    -- ExistFirstBy-WitchIsAlso-FilterOut-lemma2 : ∀ {ℓ ℓ' ℓ'' ℓ*} → {A : Type ℓ} → {B : A → Type ℓ'} → {B' : A → Type ℓ''}
    --                                                -- → { ∀ x → isProp (B x)} → { ∀ x → isProp (B' x)}
    --                                              → {B* : A → Type ℓ*} →  
    --                                               {{Dec-Pred-B : Dec-Pred B}} {{Dec-Pred-B* : Dec-Pred B*}}  → 
    --                                                  (l : List A) → (f : (x : A) → A) → -- (f : (x : A) → B x → B' x → A) →
    --                                                   ((∀ a → B a → IsEmpty (B* a)))
    --                                                  → (∀ x → B* (f x) → B* x)
    --                                                  → (z : ExistFirstBy B WitchIsAlso B' l)
    --                                                  → (z' : ExistFirstBy B WitchIsAlso B' (fst (FilterOut B* l))) → 

    --                                                  (fst (FilterOut B*
    --                                                     (map-ExistingFirstBy B WitchIsAlso B'
    --                                                        l
    --                                                          z λ x x₁ x₂ → f x)))
    --                                                    ≡
    --                                                   (map-ExistingFirstBy B WitchIsAlso B'
    --                                                      (fst (FilterOut B* l)) z' λ x x₁ x₂ → f x)                                                         
    -- ExistFirstBy-WitchIsAlso-FilterOut-lemma2 {B = B} {B' = B'} {B* = B*}  {{Dec-Pred-B* = Dec-Pred-B* }}  (x₁ ∷ l) f x g (inl x₂) with (Dec-Pred.decide Dec-Pred-B* (f x₁)) | (Dec-Pred.decide Dec-Pred-B* x₁)
    -- ... | _ | yes p₁ = empty-elim (x _ (proj₁ x₂) p₁)
    -- ... | yes p | no ¬p = empty-rec (¬p (g x₁ p)) 
    -- ... | no ¬p | no ¬p₁ =
    --    sum-elim {C = λ (z'
    --            : (B x₁ × B' x₁) ⊎
    --              ((B x₁ → Empty) ×
    --               ExistFirstBy B WitchIsAlso B' (fst (FilterOut B* l)))) →
    --           f x₁  ∷ fst (FilterOut B* l) ≡
    --           map-ExistingFirstBy B WitchIsAlso B' (x₁ ∷ fst (FilterOut B* l)) z'
    --           λ x₃ x₄ x₅ → f x₃} (λ a → refl)
    --             --(λ a → cong (_∷ fst (FilterOut B* l)) λ i → f x₁ (B-prop x₁ (proj₁ x₂) (proj₁ a) i) (B'-prop x₁ (proj₂ x₂) (proj₂ a)  i))
    --             λ b → empty-rec (proj₁ b (proj₁ x₂)) 



    -- ExistFirstBy-WitchIsAlso-FilterOut-lemma2 {B = B} {B' = B'} {B* = B*} ⦃ Dec-Pred-B* = Dec-Pred-B* ⦄ (x₁ ∷ l) f x g (inr x₂) = 
      
    --   dec-elim
    --     (λ xx →
    --          (z'
    --                 : ExistFirstBy B WitchIsAlso B'
    --                   (fst
    --                    (dec-rec' (B* x₁) (λ _ → FilterOut B* l)
    --                     (λ y →
    --                        x₁ ∷ fst (FilterOut B* l) , sum-elim y ((snd (FilterOut B* l)) ∘ proj₂))
    --                     xx))) →
    --                fst
    --                (dec-rec' (B* x₁)
    --                 (λ _ →
    --                    FilterOut B* (map-ExistingFirstBy B WitchIsAlso B' l (proj₂ x₂) λ v v₁ v₂ → f v))
    --                 (λ y →
    --                    x₁ ∷
    --                    fst
    --                    (FilterOut B*
    --                     (map-ExistingFirstBy B WitchIsAlso B' l (proj₂ x₂) λ v v₁ v₂ → f v))
    --                    ,
    --                    sum-elim y                         
    --                     (snd (FilterOut B* (map-ExistingFirstBy B WitchIsAlso B' l (proj₂ x₂) (λ v v₁ v₂ → f v))) ∘ proj₂ )

    --                      )
    --                 xx)
    --                ≡
    --                map-ExistingFirstBy B WitchIsAlso B'
    --                (fst
    --                 (dec-rec' (B* x₁) (λ _ → FilterOut B* l)
    --                  (λ y →
    --                     x₁ ∷ fst (FilterOut B* l) , sum-elim y
    --                       ((snd (FilterOut B* l)) ∘ proj₂)
    --                     )
    --                  xx))
    --                z' λ v v₁ v₂ → f v
    --          )
    --      (λ x₃ → ExistFirstBy-WitchIsAlso-FilterOut-lemma2 {B = B} {B' = B'} l f x g (proj₂ x₂))
    --      (λ x₃ → sum-elim
    --         {C = λ z* → x₁ ∷
    --                    fst
    --                    (FilterOut B*
    --                     (map-ExistingFirstBy B WitchIsAlso B' l (proj₂ x₂) λ v v₁ v₂ → f v))
    --                    ≡
    --                    map-ExistingFirstBy B WitchIsAlso B' (x₁ ∷ fst (FilterOut B* l)) z*
    --                    λ v v₁ v₂ → f v}
    --          (λ a → empty-rec (proj₁ x₂ (proj₁ a)))
    --          λ b → cong (x₁ ∷_) (ExistFirstBy-WitchIsAlso-FilterOut-lemma2 {B = B} {B' = B'} l f x g (proj₂ x₂) (proj₂ b)))
    --      ((Dec-Pred.decide Dec-Pred-B* x₁)) 


    -- map-ExistingFirstBy-lemma3 : {cs : List ContextEntry} {nm : Identifier} 
    --                    (B' : ContextEntry → Type₀) -- → (B'-prop : ∀ x → isProp)
    --                    (z : ExistFirstBy ((nm ≡_) ∘ ce-name) WitchIsAlso B' cs) →
    --                    (f : ContextEntry → Scope)
    --                    (r : Subst' cs) →                       
    --                    (SubstNotMatchEntry _ _ _ r z) →
    --                    (ex' : ExistFirstBy ((nm ≡_) ∘ ce-name) WitchIsAlso B' (remSubst' cs r)) → 
    --                    remSubst' _ (map-ExistingFirstBy-lemma ((nm ≡_) ∘ ce-name) B' z f r) ≡
    --                        map-ExistingFirstBy ((nm ≡_) ∘ ce-name) WitchIsAlso B' (remSubst' cs r)
    --                          ex' λ x x₁ x₂ → record x { scope = f x }
    -- map-ExistingFirstBy-lemma3 {x₁ ∷ cs} B' (inl x₂) f (inr x₃) x (inl x₄) = refl
    -- map-ExistingFirstBy-lemma3 {x₁ ∷ cs} B' (inl x₂) f (inr x₃) x (inr x₄) = empty-rec (proj₁ x₄ (proj₁ x₂))
    -- map-ExistingFirstBy-lemma3 {x₁ ∷ cs} {nm} B' (inr x₂) f (inl x₃) x =
    --    ExistFirstBy-WitchIsAlso-FilterOut-lemma2 {B = λ v → (_≡_ nm ∘ ce-name) v} {_}
    --      -- {λ x₄ → Discrete→isSet (IsDiscrete.eqTest IsDiscrete-Identifier) _ _}
    --      -- {λ x₄ → {!!}}
    --       {B* = _≡_ (ce-name x₁) ∘ ce-name}
    --          {{Dec-Pred-Disc {{IsDiscrete-Identifier}}}} {{Dec-Pred-Disc {{IsDiscrete-Identifier}}}}
    --                (cs) (λ x₄ → record x₄ { scope = f x₄ }) (λ a x₄ x₅ → proj₁ x₂ (x₄ ∙ (sym x₅))) (λ x₄ x₅ → x₅) (proj₂ x₂) 
      
    -- map-ExistingFirstBy-lemma3 {x₁ ∷ cs} B' (inr x₂) f (inr x₃) x (inl x₄) = empty-rec (proj₁ x₂ (proj₁ x₄))
    -- map-ExistingFirstBy-lemma3 {x₁ ∷ cs} B' (inr x₂) f (inr x₃) x (inr x₄) = 
    --   cong (x₁ ∷_) (map-ExistingFirstBy-lemma3 B' (proj₂ x₂) f (x₃) x (proj₂ x₄))

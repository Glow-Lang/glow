
{-# OPTIONS --cubical  #-}
module Glow.Simple.ProjectOut where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Foundations.CartesianKanOps

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod renaming (map to prod-map)
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool hiding (if_then_else_ ; _≟_) renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive hiding (_≟_)
import Cubical.Functions.Logic as Lo

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.Simple.AST

open import Glow.DecEqMore

open import Cubical.HITs.Interval

open import Glow.ListDecProps

open import Glow.Simple.ContextMore

module ProjectOut {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
              {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where

  prop-mode = one
  
  open AST Identifier builtIns prop-mode

  open PropMode prop-mode 

  -- open InteractionHead

  makeDishonest' : (ps : List (Identifier × ParticipantModality))
                   → PM (_ , UniqueByDec≡ proj₁ ps , isProp-UniqueBy _ _ )
                   → ∀ nm → PM ( IsHonestParticipantId {ps} nm )  
                           →  Σ (List (Identifier × ParticipantModality))
                                  λ ps → PM ( IsDishonestParticipantId {ps} nm )
                                    × PM (_ , UniqueByDec≡ proj₁ ps , isProp-UniqueBy _ _ )
  makeDishonest' l x nm w =
    ExistMemberAs-mapExisting-help {{eqTest (IsDiscrete-Identifier)}}
     l x w (λ x₁ q → (proj₁ x₁ , dishonest) , proj₁ q , refl)
       (λ a _ → refl)
       (λ a a' x₁ x₂ → sym (proj₁ x₁) ∙ proj₁ x₂)
       λ a a' x₁ x₂ x₃ → proj₁ x₁ ∙ x₂ ,
         sum-rec (λ a₁ → empty-elim (x₃ (proj₁ x₁ ∙ x₂ , (sym a₁)))) (sym) (dichotomyBool (proj₂ a))  

  makeDishonestΣ : (ih : InteractionHead) → HonestParticipantId ih 
                           →  Σ InteractionHead DishonestParticipantId 
  AST.participantsWM (fst (makeDishonestΣ ih hp)) =
     (fst (makeDishonest' (AST.participantsWM ih) (AST.uniquePtcpnts ih) (pId-nameHon hp) (pId-isInHon hp)))
  AST.parameters (fst (makeDishonestΣ ih hp)) = AST.parameters ih
  AST.uniqueParams (fst (makeDishonestΣ ih hp)) = AST.uniqueParams ih
  AST.uniquePtcpnts (fst (makeDishonestΣ ih hp)) = 
     proj₂ (snd (makeDishonest' (AST.participantsWM ih) (AST.uniquePtcpnts ih) (pId-nameHon hp) (pId-isInHon hp)))
  snd (makeDishonestΣ ih hp) =
    AST.pId ((pId-nameHon hp))
       {proj₁ (snd (makeDishonest' (AST.participantsWM ih) (AST.uniquePtcpnts ih) (pId-nameHon hp) (pId-isInHon hp)))}

  makeDishonest : (ih : InteractionHead) → HonestParticipantId ih → InteractionHead   
  makeDishonest ih = fst ∘ makeDishonestΣ ih

-- makeDishonest' l x nm w
 
  makeDishonest'-ParticipantId : ∀ pp → ∀ nm → ∀ x nm' w 
           → ⟨ IsParticipantId {map-List proj₁ pp} nm ⟩
           → ⟨ IsParticipantId {map-List proj₁ (fst (makeDishonest' pp x nm' w))} nm ⟩  
  makeDishonest'-ParticipantId (x₂ ∷ pp) nm x nm' (inl x₃) xx = xx
  makeDishonest'-ParticipantId (x₂ ∷ pp) nm (x , x₄) nm' (inr x₃) (inl x₁) = inl x₁
  makeDishonest'-ParticipantId (x₂ ∷ pp) nm (x , x₄) nm' (inr x₃) (inr x₁) =
     inr (proj₁ x₁ , (makeDishonest'-ParticipantId pp nm x₄ nm' (proj₂ x₃) (proj₂ x₁)))

  makeDishonest'-DishonestParticipantId : ∀ pp → ∀ nm → ∀ x nm' w 
           → ⟨ IsDishonestParticipantId {pp} nm ⟩
           → ⟨ IsDishonestParticipantId {(fst (makeDishonest' pp x nm' w))} nm ⟩  
  makeDishonest'-DishonestParticipantId (x₂ ∷ pp) nm x nm' (inl x₃) (inl x₁) = inl (proj₁ x₁ , refl)
  makeDishonest'-DishonestParticipantId (x₂ ∷ pp) nm x nm' (inl x₃) (inr x₁) =
     inr ((λ x₄ → proj₁ x (ExistMemberAs→ (λ a x₅ → sym (proj₁ x₄) ∙ proj₁ x₅) (λ a → _ ≟ _) (proj₂ x₁))) , (proj₂ x₁))
     --inr ((λ x₄ → {!proj₁ x₄ ∙ (sym (proj₁ x₃))!}) , (proj₂ x₁))
  makeDishonest'-DishonestParticipantId (x₂ ∷ pp) nm (x , x₄) nm' (inr x₃) (inl x₁) = inl x₁
  makeDishonest'-DishonestParticipantId (x₂ ∷ pp) nm (x , x₄) nm' (inr x₃) (inr x₁) =
     inr (proj₁ x₁ , (makeDishonest'-DishonestParticipantId pp nm x₄ nm' (proj₂ x₃) (proj₂ x₁)))


  makeDishonest-ParticipantId : (ih : InteractionHead) → ∀ hp → 
                                    ParticipantId ih → ParticipantId (makeDishonest ih hp)
  makeDishonest-ParticipantId ih hp (AST.pId name₁ {y}) =
     AST.pId name₁ {makeDishonest'-ParticipantId _ _ (AST.uniquePtcpnts ih) _ _ y}

    
  makeDishonest-DishonestParticipantId : (ih : InteractionHead) → ∀ hp → 
                                    DishonestParticipantId ih → DishonestParticipantId (makeDishonest ih hp)
  makeDishonest-DishonestParticipantId ih hp (AST.pId name₁ {y}) =
     AST.pId name₁ {makeDishonest'-DishonestParticipantId _ _ (AST.uniquePtcpnts ih) _ _ y}



  CtxTrans' : (ptps : List (Identifier × ParticipantModality))
            → Maybe (HonestParticipantId' {ptps})
            → (HonestParticipantId' {ptps})
            → DecPropΣ
  CtxTrans' [] x (AST.pId name₁ {()}) 
  CtxTrans' (x₃ ∷ ptps) nothing (_) = Unit-dp
  CtxTrans' (x₃ ∷ ptps) (just (AST.pId name₂ {inl x})) (AST.pId name₁ {inl x₁}) = Empty-dp
  CtxTrans' (x₃ ∷ ptps) (just (AST.pId name₂ {inl x})) (AST.pId name₁ {inr x₁}) = Unit-dp
  CtxTrans' (x₃ ∷ ptps) (just (AST.pId name₂ {inr x})) (AST.pId name₁ {inl x₁}) = Unit-dp
  CtxTrans' (x₃ ∷ ptps) (just (AST.pId name₂ {inr x})) (AST.pId name₁ {inr x₁}) = 
     CtxTrans' (ptps) (just (AST.pId name₂ {proj₂ x})) (AST.pId name₁ {proj₂ x₁})

  
  CtxTrans'-cases : (ptps : List (Identifier × ParticipantModality)) → ∀ {q}
            → ∀ nm
            → (z : (⟨ IsHonestParticipantId {ptps} nm ⟩))
            → (hp : (HonestParticipantId' {ptps}))
            → ⟨ CtxTrans' ptps (just (AST.pId nm {z})) hp ⟩
            → (⟨ IsHonestParticipantId  {(fst (makeDishonest' ptps q (pId-nameHon hp) (pId-isInHon hp)))} nm ⟩ )
  CtxTrans'-cases (x₁ ∷ ptps) nm (inr x₂) (AST.pId name₁ {inl x₃}) x = inr ((true≢false ∘ proj₂) , (proj₂ x₂))
  CtxTrans'-cases (x₁ ∷ ptps) nm (inl x₂) (AST.pId name₁ {inr x₃}) x = inl x₂
  CtxTrans'-cases (x₁ ∷ ptps) {q} nm (inr x₂) (AST.pId name₁ {inr x₃}) x =
     inr (proj₁ x₂ , CtxTrans'-cases (ptps) {proj₂ q} nm (proj₂ x₂) (AST.pId name₁ {proj₂ x₃}) x)

  
  CtxTrans'-cases¬ : (ptps : List (Identifier × ParticipantModality)) → ∀ {q}
            → ∀ nm
            → (z : (⟨ IsHonestParticipantId {ptps} nm ⟩))
            → (hp : (HonestParticipantId' {ptps}))
            → (⟨ CtxTrans' ptps (just (AST.pId nm {z})) hp ⟩ → Empty)
            → (⟨ IsDishonestParticipantId  {(fst (makeDishonest' ptps q (pId-nameHon hp) (pId-isInHon hp)))} nm ⟩ )
  CtxTrans'-cases¬ (x₁ ∷ ptps) nm (inr x₂) (AST.pId name₁ {inl x₃}) x = empty-elim (x _)
  CtxTrans'-cases¬ (x₁ ∷ ptps) nm (inl x₂) (AST.pId name₁ {inr x₃}) x = empty-elim (x _)
  CtxTrans'-cases¬ (x₁ ∷ ptps) {q} nm (inr x₂) (AST.pId name₁ {inr x₃}) x =
     let z = λ a → proj₁ q ((ExistMemberAs→ (λ a₁ x₄ → (sym (proj₁ a) ∙ (proj₁ x₄))) (λ _ → _ ≟ _) (proj₂ x₂)))
     in inr ( z , CtxTrans'-cases¬ (ptps) {proj₂ q} nm (proj₂ x₂) (AST.pId name₁ {proj₂ x₃}) x)
  CtxTrans'-cases¬ (x₁ ∷ ptps) {q} nm (inl x₂) (AST.pId name₁ {inl x₃}) x = inl ((proj₁ x₂) , refl)

  ctxTrans-hlp : ∀ {ih : _} {hp : HonestParticipantId ih} → (nm : Identifier)
       
       → (xx : ExistMemberAs
             (λ x₁ → (nm ≡ proj₁ x₁) × (true ≡ proj₂ x₁))
             (participantsWM ih))

       → ⟨ CtxTrans' (participantsWM ih) (just (AST.pId nm {xx})) hp ⟩
         
       → ExistMemberAs
             (λ x₁ → (nm ≡ proj₁ x₁) × (true ≡ proj₂ x₁))
             (ExistMemberAs-mapExisting (λ x₁ x₂ → proj₁ x₁ , false)
              (participantsWM ih) (AST.pId-isInHon _ _ _ hp))
  ctxTrans-hlp {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁} {AST.pId name₁ {inl x₂}} nm (inl x₃) ()
  ctxTrans-hlp {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁} {AST.pId name₁ {inl x₂}} nm (inr x₃) _ =
     inr ( true≢false ∘ proj₂ , (proj₂ x₃))
  ctxTrans-hlp {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁} {AST.pId name₁ {inr x₂}} nm (inl x₃) _ = inl x₃
  ctxTrans-hlp {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁ {yy} {x₄ , x₅}} {AST.pId name₁ {inr x₂}} nm (inr x₃) x = 
     inr ( proj₁ x₃
       , ctxTrans-hlp {AST.interactionHead (participantsWM₁) parameters₁ {yy} {x₅}} {AST.pId name₁ {proj₂ x₂}} nm (proj₂ x₃) x)


  ctxTrans-hlp-CE' : ∀ {ih : _} → ∀ {entries₁} → ∀ hp → ∀ nm → (q : ⟨ IsHonestParticipantId {participantsWM ih} nm ⟩) →  
                        Maybe
                          ⟨ ×-dp ( IsHonestParticipantId {participantsWM (makeDishonest ih hp)} nm )
                          
                           (CtxTrans' (participantsWM ih)
                                   (narrowScope ih
                                   (con entries₁ nothing) (just (pId nm {q})) tt)
                                   hp) ⟩ 
  ctxTrans-hlp-CE' {ih} {entries₁} hp nm q =
      dec-rec' _ just (const nothing)
        (proj₁ (snd ( ×-dp ( IsHonestParticipantId {participantsWM (makeDishonest ih hp)} nm )
                          
                           (CtxTrans' (participantsWM ih)
                                   (narrowScope ih
                                   (con entries₁ nothing) (just (pId nm {q})) tt)
                                   hp))) )

  ctxTrans-hlp-CE : ∀ {ih : _} → ∀ hp → ContextEntry ih → Maybe (ContextEntry (makeDishonest ih hp))
  ctxTrans-hlp-CE hp (AST.ice nothing name₁ type₁) = just (AST.ice nothing name₁ type₁)
  ctxTrans-hlp-CE ih@{AST.interactionHead (x ∷ xs) pms} hp (AST.ice (just zz@(AST.pId nm' {yy'})) name₁ type₁) =
    dec-rec' _ (just ∘ (λ b → ice (just (pId nm' {b})) name₁ type₁) ∘ ctxTrans-hlp {ih = ih} {hp = hp} nm' yy')
     (const nothing) (proj₁ (snd (CtxTrans' _ (just zz) hp )) )

  scopeTrans : ∀ {ih : _} {hp} → (sc : Scope ih) → ⟨ CtxTrans' (participantsWM ih) sc hp ⟩ → Scope (makeDishonest ih hp)
  scopeTrans {ih} {hp} nothing x = nothing
  scopeTrans {ih} {hp} (just (AST.pId name₁ {yy})) x = just (AST.pId name₁ {ctxTrans-hlp {ih} {hp} name₁ yy x })


  CtxTrans : {ih : InteractionHead} → HonestParticipantId ih → Context ih → Type₀
  CtxTrans {ih} hp Γ =
       ⟨ CtxTrans' (participantsWM ih) (scope' Γ) hp ⟩
        × Σ ℕ λ k → BTFS (λ Τ → (λ y → ⟨ isNothing-dp (scope y) ⟩ × (Τ ≡ type y) ))
            (name)
            k (filterMap (ctxTrans-hlp-CE {ih} hp) (AST.entries Γ))  
         -- List (BTF (filterMap (ctxTrans-hlp-CE {ih} hp) (AST.entries Γ)))


  ctxTrans : ∀ {ih : _} {hp} → (Γ : Context ih) → CtxTrans hp Γ → Context (makeDishonest ih hp)
  AST.entries (ctxTrans Γ x) = btfs _ _ (snd (proj₂ x))
  AST.scope' (ctxTrans {ih} {hp} Γ x) = scopeTrans {ih} {hp} (_) (proj₁ x)

  -- CtxTrans'-cases-PSOF : {ih : InteractionHead} → ∀ {entries₁}
  --           → ∀ nm
  --           → (z : (⟨ IsHonestParticipantId {participantsWM ih} nm ⟩))
  --           → (hp : (HonestParticipantId' {participantsWM ih}))
  --           → (d : ⟨ CtxTrans' (participantsWM ih) (just (AST.pId nm {z})) hp ⟩)
  --           → ∀ vNm
  --           → (d' : CtxTrans hp (con entries₁ nothing))
  --           → ⟨ IsPrivateSymbolOf {ih} (con entries₁ nothing) (AST.pId nm {z}) vNm ⟩
  --           → ⟨ IsPrivateSymbolOf {makeDishonest ih hp} ((ctxTrans {hp = hp} (con entries₁ nothing) d') )
  --                  (AST.pId nm {CtxTrans'-cases (participantsWM ih) {uniquePtcpnts ih} nm z hp d}) vNm ⟩
  -- CtxTrans'-cases-PSOF {entries₁ = e₀ ∷ entries₁} nm z hp d vNm (x₂ , d') = {!!}

  -- CtxTrans'-cases-PSOF : {ih : InteractionHead} → ∀ {entries₁}
  --           → ∀ nm
  --           → (z : (⟨ IsHonestParticipantId {participantsWM ih} nm ⟩))
  --           → (hp : (HonestParticipantId' {participantsWM ih}))
  --           → (d : ⟨ CtxTrans' (participantsWM ih) (just (AST.pId nm {z})) hp ⟩)
  --           → ∀ vNm
  --           → (d' : CtxTrans hp (con entries₁ nothing))
  --           → ⟨ IsPrivateSymbolOf {ih} (con entries₁ nothing) (AST.pId nm {z}) vNm ⟩
  --           → ⟨ IsPrivateSymbolOf {makeDishonest ih hp} ((ctxTrans {hp = hp} (con entries₁ nothing) d') )
  --                  (AST.pId nm {CtxTrans'-cases (participantsWM ih) {uniquePtcpnts ih} nm z hp d}) vNm ⟩
  -- CtxTrans'-cases-PSOF {entries₁ = e₀ ∷ entries₁} nm z hp d vNm (x₂ , d') =
  --    ? ∘ existFWIA-filter h

  --   where

  --     h' : ? → {!!}
  --     h' = {!!}

  --     h : ∀ a → recMaybe {!!} {!!} {!!}
  --     h = {!!}

     

  ctxTrans-symbol : {ih : InteractionHead} → ∀ Γ Τ hp → ∀ name₁ → ∀ d
                   → ⟨ IsDefinedSymbolOfTy Γ Τ name₁ ⟩ 
                   → ⟨ IsDefinedSymbolOfTy (ctxTrans {ih} {hp} Γ d) Τ name₁ ⟩ 

  ctxTrans-symbol = {!!}


  ctxTrans-IsNotConsensus : ∀ {ih} → ∀ {hp} → ∀ c → ∀ d
           → ⟨ IsNotConsensus c ⟩
           → ⟨ IsNotConsensus (ctxTrans {ih} {hp} c d) ⟩
  ctxTrans-IsNotConsensus (AST.con entries₁ (just (AST.pId name₁))) d x = tt

  ctxTrans-IsConsensus : ∀ {ih} → ∀ {hp} → ∀ c → ∀ d
           → ⟨ IsConsensus c ⟩
           → ⟨ IsConsensus (ctxTrans {ih} {hp} c d) ⟩
  ctxTrans-IsConsensus (AST.con entries₁ nothing) d x = tt

  -- ctxTransFld-lem-1 : ∀ {ih : _} {hp} →  (a : AST.ContextEntry' Identifier builtIns one) →
  --                           recMaybe (Lift Unit)
  --                           (λ a' →
  --                              (AST.name a ≡ AST.name a') ×
  --                              ((b : GType) →
  --                               (fst (isNothing-dp (AST.scope a)) × (b ≡ AST.type a) →
  --                                fst (isNothing-dp (AST.scope a')) × (b ≡ AST.type a'))
  --                               ×
  --                               (fst (isNothing-dp (AST.scope a')) × (b ≡ AST.type a') →
  --                                fst (isNothing-dp (AST.scope a)) × (b ≡ AST.type a))))
  --                           (ctxTrans-hlp-CE {ih} hp a)
  -- ctxTransFld-lem-1 {AST.interactionHead participantsWM₁ parameters₁} (AST.ice nothing name₁ type₁) =
  --    refl , (λ b → (idfun _) , (idfun _))
  -- ctxTransFld-lem-1 {AST.interactionHead [] parameters₁} (AST.ice (just (AST.pId name₂ {()})) name₁ type₁)
  -- ctxTransFld-lem-1 {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁} {hp = hp} (AST.ice (just zz@(AST.pId name₂ {yy})) name₁ type₁) with (proj₁ (snd (CtxTrans' _ (just zz) hp )) )
  -- ... | yes p = refl , (λ b → (idfun _) , (idfun _))
  -- ... | no ¬p = _



  ctxTransFld-lem-1 : ∀ {ih : _} {hp} → (entries₁ : List (ContextEntry ih)) → ∀ p → ∀ x₁
                                   -- → ∀ ( x₂ : ⟨ CtxTrans' (participantsWM ih) (just p) hp ⟩)
                           → BTFS-⇒ {D = GType} name (λ b x₃ →  ⟨ CtxTrans' (participantsWM ih) (scope x₃) hp ⟩ × ((b ≡ AST.type x₃)))
                                                    (λ b x₃ →  ⟨ CtxTrans' (participantsWM ih) (scope x₃) hp ⟩ × ((b ≡ AST.type x₃)))
                              entries₁
                              ((AST.entries
                                (bindingMechanics' ih (AST.con entries₁ nothing)
                                 (AST.Stmnt.bindingS
                                  (AST.BStmnt.BS-publish! p x₁)))))
  ctxTransFld-lem-1 [] p (AST.psof name₁ {()})
  ctxTransFld-lem-1 {AST.interactionHead [] parameters₁} (x ∷ entries₁) (AST.pId name₂ {()}) (AST.psof name₁ {inl x₁})
  ctxTransFld-lem-1 {AST.interactionHead (x₃ ∷ participantsWM₁) parameters₁} (x ∷ entries₁) (AST.pId name₂) (AST.psof name₁ {inl x₁}) = 
      (refl , (λ _ → prod-map (const _) (idfun _))) , (PW-refl _ entries₁ λ a → refl , (λ _ → idfun _))
  ctxTransFld-lem-1 {AST.interactionHead [] parameters₁} (x ∷ entries₁) (AST.pId name₂ {()}) (AST.psof name₁ {inr x₁}) 
  ctxTransFld-lem-1 ih@{AST.interactionHead (x₃ ∷ participantsWM₁) parameters₁} (x ∷ entries₁) p (AST.psof name₁ {inr x₁}) =
     (refl , (λ _ → idfun _)) ,
       ctxTransFld-lem-1 {ih} (entries₁) p (AST.psof name₁ {proj₂ x₁}) 

  ctxTransFld-lem-2 : ∀ {ih : _} {hp} → (entries₁ : List (ContextEntry ih)) → ∀ p → ∀ x₁
                      → ∀ ( x₂ : ⟨ CtxTrans' (participantsWM ih) (just p) hp ⟩)
                      → PW (λ x x₃ →  ⟨  (maybe-eqCase (ctxTrans-hlp-CE {ih} hp x) (ctxTrans-hlp-CE {ih} hp x₃)) ⟩ 
                                  × recMaybe Unit* (λ x₄ → ((AST.name x ≡ AST.name x₄)) ×
                                        (∀ b → ⟨ (isNothing-dp (AST.scope x₄)) ⟩ × (b ≡ AST.type x₄)
                                             → ⟨ (CtxTrans' (participantsWM ih) (AST.scope x) hp) ⟩ × (b ≡ AST.type x)))
                                     ( ctxTrans-hlp-CE {ih} hp x )
                                  × recMaybe Unit* (λ x₄ → ((AST.name x₃ ≡ AST.name x₄)) ×
                                        (∀ b → ⟨  (CtxTrans' (participantsWM ih) (AST.scope x₃) hp) ⟩ × (b ≡ AST.type x₃)
                                             → ⟨ (isNothing-dp (AST.scope x₄)) ⟩  × (b ≡ AST.type x₄)))
                                      (ctxTrans-hlp-CE {ih} hp x₃))
                          entries₁
                          ((AST.entries
                                (bindingMechanics' ih (AST.con entries₁ nothing)
                                 (AST.Stmnt.bindingS
                                  (AST.BStmnt.BS-publish! p x₁)))))
  ctxTransFld-lem-2 {AST.interactionHead [] parameters₁} entries₁ (AST.pId name₁ {()}) x₁ x₂ 
  ctxTransFld-lem-2 {AST.interactionHead (x ∷ participantsWM₁) parameters₁} [] p (AST.psof name₁ {()}) x₂
  ctxTransFld-lem-2 {AST.interactionHead (x ∷ participantsWM₁) parameters₁} {hp} (AST.ice nothing name₂ type₁ ∷ entries₁) p (AST.psof name₁ {inl (x₁ , ())}) x₂
     -- (tt , ((refl , (λ b x₃ → x₃)) , (refl , (λ b x₃ → x₃)))) ,
     --   (PW-refl _ entries₁ (λ a → maybe-eqCase-refl (ctxTrans-hlp-CE hp a)
     --        , {!!}))
  ctxTransFld-lem-2 {AST.interactionHead (x ∷ participantsWM₁) parameters₁} (AST.ice nothing name₂ type₁ ∷ entries₁) p (AST.psof name₁ {inr x₁}) x₂ =
    {!!} , (ctxTransFld-lem-2 {AST.interactionHead (x ∷ participantsWM₁) parameters₁} (entries₁) p (AST.psof name₁ {proj₂ x₁}) x₂)
  ctxTransFld-lem-2 {AST.interactionHead (x ∷ participantsWM₁) parameters₁} {AST.pId name₄ {yy}} (AST.ice (just (AST.pId name₃ {yyy})) name₂ type₁ ∷ entries₁) (AST.pId name₅) (AST.psof name₁ {inl x₁}) x₂ with (proj₁ (snd (CtxTrans' (x ∷ participantsWM₁) (just (pId name₃ {yyy})) (pId name₄ {yy}))))
  ... | yes p₁ = (_ , ((refl , (λ b x₃ → empty-elim (proj₁ x₃))) , (refl , (λ b x₃ → x₃)))) ,
                      (PW-refl _ entries₁
                        (λ a → maybe-elim {B = λ mmm → fst
                          (maybe-eqCase (ctxTrans-hlp-CE (pId name₄) a)
                           mmm)
                          ×
                          recMaybe (Lift Unit)
                          (λ x₄ →
                             (AST.name a ≡ AST.name x₄) ×
                             ((b : GType) →
                              fst (maybe-eqCase nothing (AST.scope x₄)) × (b ≡ AST.type x₄) →
                              fst (CtxTrans' (x ∷ participantsWM₁) (AST.scope a) (pId name₄)) ×
                              (b ≡ AST.type a)))
                          mmm
                          ×
                          recMaybe (Lift Unit)
                          (λ x₄ →
                             (AST.name a ≡ AST.name x₄) ×
                             ((b : GType) →
                              fst (CtxTrans' (x ∷ participantsWM₁) (AST.scope a) (pId name₄)) ×
                              (b ≡ AST.type a) →
                              fst (maybe-eqCase nothing (AST.scope x₄)) × (b ≡ AST.type x₄)))
                          mmm} {!!} {!!} (ctxTrans-hlp-CE (pId name₄) a)))
  ... | no ¬p = empty-elim (¬p {!x₂!})

  ctxTransFld-lem-2 {AST.interactionHead (x ∷ participantsWM₁) parameters₁} (AST.ice (just (AST.pId name₃)) name₂ type₁ ∷ entries₁) p (AST.psof name₁ {inr x₁}) x₂ =
    {!!} , (ctxTransFld-lem-2 {AST.interactionHead (x ∷ participantsWM₁) parameters₁} (entries₁) p (AST.psof name₁ {proj₂ x₁}) x₂)
  -- ctxTransFld-lem-2 ih@{AST.interactionHead (x ∷ participantsWM₁) parameters₁} {hp} (x₃ ∷ entries₁) p (AST.psof name₁ {inr x₁}) x₂ with (ctxTrans-hlp-CE {ih} hp x₃)
  -- ... | nothing = (_ , (_ , _)) , ctxTransFld-lem-2 {ih} {hp} entries₁ p (AST.psof name₁ {proj₂ x₁}) x₂
  -- ... | just x₄ = (_ , (({!!} , {!!}) , {!!})) , (ctxTransFld-lem-2 {ih} {hp} entries₁ p (AST.psof name₁ {proj₂ x₁}) x₂)
  --    -- (maybe-eqCase-refl (ctxTrans-hlp-CE hp x₃) , {!!}) , {!!}

  -- ctxTransFld-lem-1 {AST.interactionHead participantsWM₁ parameters₁} (AST.ice nothing name₁ type₁) =
  --    refl , (λ b → (idfun _) , (idfun _))
  -- ctxTransFld-lem-1 {AST.interactionHead [] parameters₁} (AST.ice (just (AST.pId name₂ {()})) name₁ type₁)
  -- ctxTransFld-lem-1 {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁} {hp = hp} (AST.ice (just zz@(AST.pId name₂ {yy})) name₁ type₁) with (proj₁ (snd (CtxTrans' _ (just zz) hp )) )
  -- ... | yes p = refl , (λ b → (idfun _) , (idfun _))
  -- ... | no ¬p = _



  -- ctxTransFld-lem-22 : ∀ ih → ∀ hp → ∀ name₃ → ∀ scope₁ → ∀ type₁ → ⟨ CtxTrans' (participantsWM ih) scope₁ hp ⟩
  --       → ⟨ maybe-eqCase (ctxTrans-hlp-CE {ih} hp (AST.ice scope₁ name₃ type₁))
  --                 (ctxTrans-hlp-CE {ih} hp (AST.ice nothing name₃ type₁)) ⟩ 
  -- ctxTransFld-lem-22 ih hp name₃ nothing type₁ x = _
  -- ctxTransFld-lem-22 (AST.interactionHead [] parameters₁) hp name₃ (just (AST.pId name₁ {()})) type₁ x
  -- ctxTransFld-lem-22 (AST.interactionHead (x₂ ∷ participantsWM₁) parameters₁) hp name₃ (just (AST.pId name₁ {yy})) type₁ x with (proj₁ (snd (CtxTrans' (x₂ ∷ participantsWM₁) (just (pId name₁ {yy})) hp)))
  -- ... | yes p = _
  -- ... | no ¬p = ¬p x
  
  -- ctxTransFld-lem-2 : ∀ {ih : _} {hp} → ∀ entries₁
  --                                → (p : HonestParticipantId')
  --                                → (x₁ : PrivateSymbolOf (AST.con entries₁ nothing) p) →
  --                                   (x₂ : fst (CtxTrans' (participantsWM ih) (just p) hp))
  --                                   → PW (λ x₃ x₄ →
  --                                     Lift
  --                                     ⟨(maybe-eqCase (ctxTrans-hlp-CE {ih} hp x₃) (ctxTrans-hlp-CE {ih} hp x₄))⟩)
  --                                  entries₁ (bindingMechanics ih {AST.con entries₁ nothing} (BS-publish! p x₁))
  -- ctxTransFld-lem-2 {AST.interactionHead [] parameters₁} {hp} (AST.ice scope₁ name₃ type₁ ∷ entries₁) (AST.pId name₂ {()}) (AST.psof name₁ {inl x₁}) x₂

  -- ctxTransFld-lem-2 {ih@(AST.interactionHead (xx ∷ participantsWM₁) parameters₁)} {hp} (AST.ice nothing name₃ type₁ ∷ entries₁) (AST.pId name₂ {yy}) (AST.psof name₁ {inl (x , ())}) x₂
  -- ctxTransFld-lem-2 {ih@(AST.interactionHead (xx ∷ participantsWM₁) parameters₁)} {hp} (AST.ice ss@(just (AST.pId name₄ {yy'})) name₃ type₁ ∷ entries₁) (AST.pId name₂ {yy}) (AST.psof name₁ {inl x₁}) x₂ = 
  --   (lift (ctxTransFld-lem-22 ih hp name₃ ss type₁
  --         -- {!x₂!} 
  --       (subst {x = (pId name₂ {yy})} {y = (pId name₄ {yy'})}
  --         (λ x → fst (CtxTrans' (xx ∷ participantsWM₁) (just x) hp)) (λ i → AST.pId (proj₂ x₁ i)
  --                    {toPathP {A = λ i → fst (IsHonestParticipantId {participants = xx ∷ participantsWM₁} (proj₂ x₁ i))}
  --                      {x = yy} {y = yy'}
  --                       (proj₂ (snd (IsHonestParticipantId {xx ∷ participantsWM₁} (proj₂ x₁ i1))) _ _)
  --                               i}) x₂ )
  --      )) , PW-refl _ entries₁
  --            λ a → lift (maybe-eqCase-refl (ctxTrans-hlp-CE hp a))
  
  -- ctxTransFld-lem-2 {hp = hp} (x ∷ entries₁) p (AST.psof name₁ {inr x₁}) x₂ =
  --    lift (maybe-eqCase-refl (ctxTrans-hlp-CE hp x)) , (ctxTransFld-lem-2 (entries₁) p (AST.psof name₁ {proj₂ x₁}) x₂)

  ctxTransFld : ∀ {ih : _} {hp} → (c : Context ih) (a : Stmnt ih c)
                    → CtxTrans hp c
                    → CtxTrans hp (bindingMechanics' ih c a)
  ctxTransFld {hp = hp} c (AST.bindingS (AST.BS-let (AST.ice scope₁ name₁ type₁) x₁)) x = 
    (proj₁ x) , 
      maybe-elim
        {B = λ x₂ →
             Σ ℕ
                (λ k →
                   BTFS (λ Τ y → typ (isNothing-dp (AST.scope y)) × (Τ ≡ AST.type y))
                   AST.name k
                   (recMaybe (filterMap (ctxTrans-hlp-CE hp) (c .AST.entries))
                    (_∷ filterMap (ctxTrans-hlp-CE hp) (c .AST.entries))
                    x₂))
                 }
                 (proj₂ x)
                 (λ _ →  _ , tailBTFS _ _ (snd (proj₂ x)))
                  (ctxTrans-hlp-CE hp (ice scope₁ name₁ type₁)) 
  ctxTransFld {ih} {hp = hp} c@(AST.con entries₁ nothing) (AST.bindingS (AST.BS-publish! p x₁ {isCon})) x = 
       (proj₁ x) , 
      dec-rec' _
          (λ x₂ → (fst (proj₂ x))
             , btfs-⇒ name
                 (BTFS-⇒-filterMap name name
                      -- {B' = {!λ z z₁ →
                      --           Cubical.Syntax.⟨⟩.has-⟨⟩.⟨ TypeWithStr-has-⟨⟩ ⟩
                      --           (CtxTrans' (participantsWM ih) (AST.scope z₁) hp)
                      --           × (z ≡ AST.type z₁)!}}
                      -- {B'' = {!!}} 
                   (ctxTransFld-lem-2 {ih} {hp} entries₁ p x₁ x₂)
                    ((ctxTransFld-lem-1 {ih} {hp} entries₁ p x₁)))
                 (fst (proj₂ x)) (snd (proj₂ x))  
                -- btfs-⇒ _
                --       (btfs-map _ (BTFS-⇒-filterMap name name {f = (ctxTrans-hlp-CE hp)}
                --             {entries₁}
                --             {bindingMechanics ih (BS-publish! p x₁ {isCon})}
                --           (ctxTransFld-lem-2 entries₁ p x₁ x₂)
                --            {B' = λ x₃ x₄ → ⟨  CtxTrans' (participantsWM ih) (AST.scope x₄) hp  ⟩ × (x₃ ≡ AST.type x₄)}
                --            {B'' = λ x₃ x₄ → ⟨ CtxTrans' (participantsWM
                --                                           (makeDishonest (ih) hp)) (AST.scope x₄)
                --                                              ({!ih!}) ⟩ × (x₃ ≡ AST.type x₄)}
                --              -- {!!}
                --            (ctxTransFld-lem-1 {ih} {hp})
                --            {!(snd (proj₂ x))!}))
                           
                --                               (fst (proj₂ x)) (snd (proj₂ x))
             )
          {!!}
         -- (λ x₂ → map-List (BTF-trans
         --  (haveSameL-filter-lemma
         --                                 (ctxTrans-hlp-CE hp)
         --                                 entries₁
         --                                 (bindingMechanics ih (BS-publish! p x₁ {isCon}))
         --                                 {!!})

         --  ) (proj₂ x))

         -- (λ x₂ → {!!}
         --      ∷ map-List {!!} (proj₂ x))
           ((proj₁ (snd (CtxTrans' _ (just p) hp )) ))
  ctxTransFld c (AST.nonBindingS x₁) x = x

  -- TODO : remove unsafe pragma by stratification on nesting depth or by introducing speicalized subst in h-expr

--   {-# TERMINATING #-}
--   paramSubst : ∀ {ih : _} → (hp : HonestParticipantId ih)  → 
--                    ∀ {Γ : Context ih} → (t : CtxTrans hp Γ) →  Statements _ Γ → Statements _ (ctxTrans {hp = hp} Γ t) 


--   paramSubst-e : ∀ {ih : _} → (hp : HonestParticipantId ih) → ∀ Τ → {c : Context ih} (d : CtxTrans hp c)
--                    → Expr ih c Τ → (Expr (makeDishonest ih hp) (ctxTrans {ih} {hp} c d) Τ)


--   paramSubst-h-arg : ∀ {ih : _} → (hp : HonestParticipantId ih) → ∀ Τ → {c : Context ih} (d : CtxTrans hp c)
--                    → Arg ih c Τ → (Arg (makeDishonest ih hp) (ctxTrans {ih} {hp} c d) Τ)

--   paramSubst-h-args : ∀ {ih : _} → (hp : HonestParticipantId ih) → ∀ Τs → {c : Context ih} (d : CtxTrans hp c)
--                    → Args ih c Τs → (Args (makeDishonest ih hp) (ctxTrans {ih} {hp} c d) Τs)


--   paramSubst-h : ∀ {ih : _} → (hp : HonestParticipantId ih) → {c : Context ih} (d : CtxTrans hp c) → Stmnt ih c → Maybe (Stmnt (makeDishonest ih hp) (ctxTrans {ih} {hp} c d))
--   paramSubst-h {AST.interactionHead [] parameters₁} (AST.pId name₁ {()}) (x₁ , x₂) x
--   paramSubst-h {AST.interactionHead (x ∷ participantsWM₁) parameters₁} hp {AST.con entries₁ nothing} d (set name₁ ∶ type₁ ≔ x₁) =
--       just (AST.bindingS (AST.BS-let (AST.ice nothing name₁ type₁) (paramSubst-e hp type₁ d x₁)))
--   paramSubst-h ih@{AST.interactionHead (x ∷ participantsWM₁) parameters₁} hp {AST.con entries₁ nothing} d (AST.bindingS (AST.BS-let (AST.ice (just (AST.pId name₂ {yy})) name₁ type₁) x₁)) =
  
--        map-Maybe (λ zz → ((AST.bindingS (AST.BS-let (AST.ice (just (AST.pId name₂)) name₁ type₁)
--          ((paramSubst-e hp type₁ (proj₂ zz , proj₂ d) x₁)))))) (ctxTrans-hlp-CE' {ih} {entries₁} hp name₂ yy)
--   -- 
--   paramSubst-h {AST.interactionHead (x ∷ participantsWM₁) parameters₁} hp {AST.con entries₁ (just (AST.pId name₂))} d (set name₁ ∶ type₁ ≔ x₁) = 
--     just (AST.bindingS (AST.BS-let (AST.ice nothing name₁ type₁) (paramSubst-e hp type₁ d x₁)))


--   paramSubst-h {ih@(AST.interactionHead (x ∷ participantsWM₁) parameters₁)} hp {c@(AST.con entries₁ nothing)} d (AST.bindingS (AST.BS-publish! p@(AST.pId name₁ {yy}) x₁)) =
--    just ( dec-rec' _
--               {!!}
--               {!!}
--               -- (λ x → (AST.bindingS
--               --            ((AST.BS-publish!
--               --               (AST.pId _ {CtxTrans'-cases _ {AST.uniquePtcpnts ih} _ (pId-isInHon p) hp x})
--               --                          (AST.psof _
--               --                            {CtxTrans'-cases-PSOF _
--               --                                   (pId-isInHon p) _ x _ d (psof-proof _ x₁)  })
--               --                          ))))
--               -- (λ x₃ → (AST.bindingS (AST.BS-let (AST.ice nothing (psof-name _ x₁)
--               --                    (IsPrivateSymbolOf→GType ih c p _ (psof-proof _ x₁)))
--               --                    (receivePublished (AST.pId _
--               --                          {CtxTrans'-cases¬ _ {AST.uniquePtcpnts ih} _ (pId-isInHon p) hp x₃})) )))
--            (proj₁ (snd (CtxTrans' _ (just p) hp)))
--      )
--   paramSubst-h ih@{AST.interactionHead (x ∷ participantsWM₁) parameters₁} hp c@{AST.con entries₁ nothing} d (AST.nonBindingS (AST.stmntNBS x₁)) = just (h x₁)
--      where
--        h : NBStmnt
--            (interactionHead (x ∷ participantsWM₁) parameters₁) c → 
--                     (Stmnt
--                      (makeDishonest (interactionHead (x ∷ participantsWM₁) parameters₁)
--                       hp)
--                      (ctxTrans c d))
--        h (AST.NBS-require! x) = (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! (paramSubst-e hp _ d x))))
--        h (AST.NBS-deposit! x x₁) = (AST.nonBindingS (AST.stmntNBS (AST.NBS-deposit! (makeDishonest-ParticipantId ih hp x) (paramSubst-e hp _ d x₁)))) 
--        h (AST.NBS-withdraw! x x₁) = (AST.nonBindingS (AST.stmntNBS (AST.NBS-withdraw! (makeDishonest-ParticipantId ih hp x) (paramSubst-e hp _ d x₁))))
--        h (AST.NBS-publishVal! x x₁) = imposible-statment-ProjectOut
--          where
--            postulate imposible-statment-ProjectOut : _

       
--   paramSubst-h {AST.interactionHead (x ∷ participantsWM₁) parameters₁} hp {c} d (AST.nonBindingS (AST.exprNBS x₁)) =
--      just (AST.nonBindingS (AST.exprNBS (paramSubst-e hp _ d x₁)))
    
  
--   paramSubst-h-args hp [] d x = _
--   paramSubst-h-args hp (x₁ ∷ []) d x = paramSubst-h-arg hp x₁ d x
--   paramSubst-h-args hp (x₁ ∷ x₂ ∷ Τs) d (x , x₃) = paramSubst-h-arg hp x₁ d x , paramSubst-h-args hp (x₂ ∷ Τs) d x₃

--   paramSubst-h-arg hp Τ d (AST.var-a (AST.dsot name₁ {yy})) =
--      AST.var-a (AST.dsot name₁ {ctxTrans-symbol _ _ _ _ _ yy})
--   paramSubst-h-arg hp Τ d (AST.lit-a x) = AST.lit-a x

--   paramSubst-e hp Τ d (AST.var (AST.dsot name₁ {yy})) =
--     AST.var (AST.dsot name₁ {ctxTrans-symbol _ _ _ _ _ yy})
  
--   paramSubst-e {ih} hp Τ {Γ} d (stmnts₁ ;b expr₁) =
--     let stmnts₁' = paramSubst hp d stmnts₁
--         (Γ' , d') = foldLinked' (Linked'-collect-Σ {fld-D = ctxTransFld {ih} {hp}} d stmnts₁ )
--         p : (fst (foldLinked' (Linked'-collect-Σ d stmnts₁))) ≡ (foldLinked' stmnts₁)
--         p = {!!}
--         expr₁' = paramSubst-e hp Τ (subst (CtxTrans hp) p d') expr₁
--         w' : ctxTrans (foldLinked' stmnts₁) (subst (CtxTrans hp) p d')
--                ≡ (foldLinked' (paramSubst hp d stmnts₁))
--         w' = {!!}
--     in stmnts₁' ;b  subst (λ x → Expr (makeDishonest ih hp) x Τ) w' expr₁'
    
--   paramSubst-e hp Τ d (AST.lit x) = lit x
--   paramSubst-e hp Τ d (x AST.$' x₁) =
--      (x AST.$' paramSubst-h-args hp _ d x₁)
--   paramSubst-e hp Τ d (AST.input x {y}) = (AST.input x {ctxTrans-IsNotConsensus _ d y})
--   paramSubst-e hp Τ d (AST.sign x {y} {yy}) = (AST.sign (paramSubst-h-arg hp _ d x) {ctxTrans-IsNotConsensus _ d y } {yy})
--   paramSubst-e {ih} hp Τ d (AST.receivePublished x {y}) =
--       (AST.receivePublished (makeDishonest-DishonestParticipantId ih hp x) {ctxTrans-IsConsensus _ d y})
--   paramSubst-e hp Τ d (AST.if x then x₁ else x₂) =
--       (AST.if (paramSubst-e hp _ d x) then (paramSubst-e hp Τ d x₁) else (paramSubst-e hp Τ d x₂))





--   paramSubst {ih} hp t =
--        map-Linked'-map-Σ-Mb {D = CtxTrans {ih} hp}
--          (ctxTransFld {ih} {hp})
--          (ctxTrans {ih} {hp})
--           (paramSubst-h {ih} hp)
--          {!!}
--          t


-- -- --       h : {Γ : Context ih}
-- -- --              → (b : Stmnt ih Γ) → Stmnt _ (stripParamsCtx Γ)

-- -- --       h-expr : {Γ : Context ih} → ∀ {Τ}
-- -- --              → (b : Expr ih Γ Τ) → Expr _ (stripParamsCtx Γ) Τ

-- -- --       h-arg : ∀ {Γ Τ} → Arg ih Γ Τ → Arg _ (stripParamsCtx Γ) Τ
-- -- --       h-arg (AST.var-a (AST.dsot x {y})) =
-- -- --           sum-elim
-- -- --            (λ a → var-a (dsot x {inl a}))
-- -- --            (lit-a ∘ (lookup-ParametersValue (ih .parameters) vv (iwt x _)) ∘ proj₂)
-- -- --             y
-- -- --       h-arg (AST.lit-a x) = (AST.lit-a x)


-- -- --       h-args : ∀ {Γ Τs}  → Args ih Γ Τs → Args _ (stripParamsCtx Γ) Τs
-- -- --       h-args {Τs = []} x = tt
-- -- --       h-args {Τs = x₁ ∷ []} x = h-arg x
-- -- --       h-args {Τs = x₁ ∷ x₂ ∷ Τs} (x , x₃) = h-arg x , h-args  x₃ 
      

-- -- --       h  (bindingS x) = bindingS (BS-lemma x)
-- -- --          where
-- -- --               BS-lemma : {Γ : Context ih} →  BStmnt ih Γ -> BStmnt _ (stripParamsCtx Γ)
-- -- --               BS-lemma (BS-let x {asn} y) = (BS-let x {asn} (h-expr y))  
-- -- --               BS-lemma (BS-publish! p (psof name₁ {w}) {y}) = (BS-publish! p (psof name₁ {w}) {y})


-- -- --       h (nonBindingS x) = nonBindingS (z x)
-- -- --          where

-- -- --            zz : NBStmnt _ _ → NBStmnt _ _ 
-- -- --            zz (NBS-require! x) = NBS-require! (h-expr x)
-- -- --            zz (NBS-deposit! p {y} x) = NBS-deposit! p {y} (h-expr x)
-- -- --            zz (NBS-withdraw! p {y} x) = NBS-withdraw! p {y} (h-expr x)
-- -- --            zz (NBS-publishVal! x y {z}) = NBS-publishVal! x y {z}

-- -- --            z : NBStmnt+Expr ih _ → NBStmnt+Expr (stripParamsHead ih) _
-- -- --            z (stmntNBS x) =  stmntNBS (zz x)
-- -- --            z (exprNBS x) = exprNBS (h-expr x)

-- -- --       h-expr (var (dsot x {y})) =
-- -- --          sum-elim
-- -- --            (λ a → var (dsot x {inl a}))
-- -- --            (lit ∘ (lookup-ParametersValue (ih .parameters) vv (iwt x _)) ∘ proj₂)
-- -- --             y



-- -- --       h-expr (stmnts₁ ;b x) =
-- -- --          paramSubst {ih = ih} vv stmnts₁ ;b subst (λ x₁ → Expr _ x₁ _)
-- -- --              -- TODO : improve evaluation performance by introducing specialized "subst"
-- -- --              -- specialisation should be not only on Expr, but also on map-Linked'-map-fold
-- -- --         (map-Linked'-map-fold (stripParamsCtx {ih}) _ _ stmnts₁ ) (h-expr x)
-- -- --       h-expr (lit x) = lit x
-- -- --       h-expr (_$'_ f xs) = _$'_ f (h-args xs)

-- -- --       h-expr (input msg {y}) = input msg {y}
-- -- --       -- h-expr (receivePublished x {y}) = publishVal x {y}
-- -- --       h-expr (if b then t else f) = if (h-expr b) then (h-expr t) else (h-expr f)
-- -- --       h-expr (AST.sign q {y} {w}) = (AST.sign (h-arg q) {y} {w})
-- -- --       h-expr (AST.receivePublished x x₁ {y}) = AST.receivePublished x x₁ {y}

-- -- --       hh : (Γ : Context ih) (x : Stmnt _ Γ) →
-- -- --          stripParamsCtx (bindingMechanics' _ Γ x) ≡
-- -- --          bindingMechanics' (interactionHead (participantsWM ih) [])
-- -- --          (stripParamsCtx Γ) (h x)
-- -- --       hh _ (bindingS (BS-let _ _)) = refl 
-- -- --       hh _ (AST.bindingS (AST.BS-publish! _ (AST.psof name₁))) = refl
-- -- --       hh _ (nonBindingS _) = refl

-- -- --       -- h-args = ?

-- -- -- -- module Test-String where
-- -- -- --   open AST String {{String-Discrete-postulated}} zero

-- -- -- --   module ParamsSubstS = ParamsSubst {{String-Discrete-postulated}}

-- -- -- --   someInteraction : Interaction 
-- -- -- --   someInteraction =  
-- -- -- --        interaction⟨   "A" ∷ "B" ∷ [] ,  "pI1" ∶ Nat ∷ "b2" ∶ Bool ∷ "b1" ∶ Bool ∷ [] ⟩ (
-- -- -- --             set "x" ∶ Bool ≔ < true > ;
-- -- -- --             at "B" set "y" ∶ Bool ≔ v "b1" ;
-- -- -- --             at "A" set "xx" ∶ Bool ≔
-- -- -- --              ( if v "b1"
-- -- -- --                then
-- -- -- --                   (
-- -- -- --                   set "z" ∶ Bool ≔ input "enter choice 1" ;₁ ;b
-- -- -- --                   v "z"
-- -- -- --                 )
-- -- -- --                else (
-- -- -- --                 require! v "b2" ;'
-- -- -- --                 -- publish! "B" ⟶ "y" ;
-- -- -- --                 -- withdraw! "B" ⟵ < 3 > ;
-- -- -- --                 -- deposit! "B" ⟶ < 2 > ;
-- -- -- --                 set "z" ∶ Bool ≔ < false > ;b
-- -- -- --                 < true >
-- -- -- --                 )) ;
-- -- -- --             deposit! "B" ⟶ < 2 > ;
-- -- -- --             at "A" set "yq" ∶ Bool ≔ input "enter choice 2" ;
-- -- -- --             withdraw! "B" ⟵ < 3 > ;
-- -- -- --             publish! "A" ⟶ "xx" ;        

-- -- -- --             publish! "B" ⟶ "y" ;'        
-- -- -- --             set "yy" ∶ Bool ≔ v "y" )


-- -- -- --   param-sub-test : ℕ × 𝟚 × 𝟚 × Unit → Linked'
-- -- -- --                                         (bindingMechanics'
-- -- -- --                                          (ParamsSubstS.stripParamsHead
-- -- -- --                                           (interactionHead ("A" ∷ "B" ∷ [])
-- -- -- --                                            ("pI1" ∶ Nat ∷ "b2" ∶ Bool ∷ "b1" ∶ Bool ∷ []))))
-- -- -- --                                         (ParamsSubstS.stripParamsCtx (Interaction.emptyContext someInteraction))
-- -- -- --   param-sub-test vv = ParamsSubstS.paramSubst vv (Interaction.code someInteraction)
-- -- -- --       -- {!ParamsSubstS.paramSubst vv (Interaction.code someInteraction)!}


-- -- -- --   zzz :
-- -- -- --     let q : ℕ × 𝟚 × 𝟚 × Unit
-- -- -- --         q = 3 , false , true , _
-- -- -- --         bT : Statements _ _
-- -- -- --         bT = (
-- -- -- --           set "x" ∶ Bool ≔ < true > ;
-- -- -- --           at "B" set "y" ∶ Bool ≔ < true > ;
-- -- -- --           at "A" set "xx" ∶ Bool ≔ (
-- -- -- --               require! < false > ;'
-- -- -- --               -- publish! "B" ⟶ "y" ;
-- -- -- --               -- withdraw! "B" ⟵ < 3 > ;
-- -- -- --               -- deposit! "B" ⟶ < 2 > ;
-- -- -- --               set "z" ∶ Bool ≔ < false > ;b
-- -- -- --               < true >
-- -- -- --               );
-- -- -- --           deposit! "B" ⟶ < 2 > ;
-- -- -- --           withdraw! "B" ⟵ < 3 > ;
-- -- -- --           publish! "B" ⟶ "y" ;'        
-- -- -- --           set "yy" ∶ Bool ≔ v "y"
-- -- -- --           )
-- -- -- --     in bT ≡ param-sub-test q 

-- -- -- --   zzz = refl



-- -- -- -- module Test-ℕ where
-- -- -- --   open AST ℕ 

-- -- -- --   module ParamsSubstS = ParamsSubst {ℕ}

-- -- -- --   someInteraction : Interaction
-- -- -- --   someInteraction =  
-- -- -- --      interaction⟨   1 ∷ 2 ∷ [] ,  3 ∶ Nat ∷ 4 ∶ Bool ∷ 5 ∶ Bool ∷ [] ⟩ (
-- -- -- --           set 6 ∶ Bool ≔ < true > ;
-- -- -- --           at 2 set 7 ∶ Bool ≔ v 5 ;
-- -- -- --           at 1 set 8 ∶ Bool ≔ (
-- -- -- --               require! v 4 ;'
-- -- -- --               -- publish! "B" ⟶ "y" ;
-- -- -- --               -- withdraw! "B" ⟵ < 3 > ;
-- -- -- --               -- deposit! "B" ⟶ < 2 > ;
-- -- -- --               set 9 ∶ Bool ≔ < false > ;b
-- -- -- --               < true >
-- -- -- --               );
-- -- -- --           deposit! 2 ⟶ < 2 > ;
-- -- -- --           withdraw! 2 ⟵ < 3 > ;
-- -- -- --           publish! 2 ⟶ 7 ;'        
-- -- -- --           set 10 ∶ Bool ≔ v 7 )


-- -- -- --   param-sub-test : ℕ × 𝟚 × 𝟚 × Unit → Linked'
-- -- -- --                                         (bindingMechanics'
-- -- -- --                                          (ParamsSubstS.stripParamsHead
-- -- -- --                                           (interactionHead (1 ∷ 2 ∷ [])
-- -- -- --                                            (3 ∶ Nat ∷ 4 ∶ Bool ∷ 5 ∶ Bool ∷ []))))
-- -- -- --                                         (ParamsSubstS.stripParamsCtx (Interaction.emptyContext someInteraction))
-- -- -- --   param-sub-test vv = ParamsSubstS.paramSubst vv (Interaction.code someInteraction)

-- -- -- --   zzz-0 : Linked'
-- -- -- --             (bindingMechanics'
-- -- -- --              (ParamsSubstS.stripParamsHead
-- -- -- --               (interactionHead (1 ∷ 2 ∷ [])
-- -- -- --                (3 ∶ Nat ∷ 4 ∶ Bool ∷ 5 ∶ Bool ∷ []))))
-- -- -- --             (ParamsSubst.stripParamsCtx
-- -- -- --              (Interaction.emptyContext someInteraction))
              
-- -- -- --   zzz-0 = param-sub-test (3 , false , true , _)
-- -- -- --            -- bindingS
-- -- -- --             -- (BS-let (AST.ice nothing 6 Bool) {_}
-- -- -- --             --  (lit true))
-- -- -- --             -- ∷L
-- -- -- --             -- (bindingS
-- -- -- --             --  (BS-let
-- -- -- --             --   (transp {λ i → ℓ-zero}
-- -- -- --             --    (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --    (AST.ice (just (AST.pId 2 {_})) 7 Bool))
-- -- -- --             --   {_} (lit true))
-- -- -- --             --  ∷L
-- -- -- --             --  (bindingS
-- -- -- --             --   (BS-let
-- -- -- --             --    (transp {λ i → ℓ-zero}
-- -- -- --             --     (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --     (transp {λ i → ℓ-zero}
-- -- -- --             --      (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --      (AST.ice (just (AST.pId 1 {_})) 8 Bool)))
-- -- -- --             --    {_}
-- -- -- --             --    (body
-- -- -- --             --     (transp {λ i → ℓ-zero}
-- -- -- --             --      (λ i →
-- -- -- --             --         AST.Body {ℕ} ⦃ ℕ-Discrete ⦄
-- -- -- --             --         (AST.interactionHead (1 ∷ 2 ∷ []) [] {_})
-- -- -- --             --         (record
-- -- -- --             --          { entries =
-- -- -- --             --              transp {λ i₁ → ℓ-zero}
-- -- -- --             --              (λ i₁ → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) (~ i)
-- -- -- --             --              (AST.ice (just (AST.pId 2 {_})) 7 Bool)
-- -- -- --             --              ∷ AST.ice nothing 6 Bool ∷ []
-- -- -- --             --          ; scope' = just (AST.pId 1 {_})
-- -- -- --             --          })
-- -- -- --             --         Bool)
-- -- -- --             --      i0
-- -- -- --             --      (transp {λ i → ℓ-zero}
-- -- -- --             --       (λ i →
-- -- -- --             --          AST.Body {ℕ} ⦃ ℕ-Discrete ⦄
-- -- -- --             --          (AST.interactionHead (1 ∷ 2 ∷ []) [] {_})
-- -- -- --             --          (record
-- -- -- --             --           { entries =
-- -- -- --             --               AST.ice (just (AST.pId 2 {_})) 7 Bool ∷ AST.ice nothing 6 Bool ∷ []
-- -- -- --             --           ; scope' = just (AST.pId 1 {_})
-- -- -- --             --           })
-- -- -- --             --          Bool)
-- -- -- --             --       i0
-- -- -- --             --       (bodyR
-- -- -- --             --        (nonBindingS
-- -- -- --             --         (stmntNBS
-- -- -- --             --          (NBS-require! (lit false)))
-- -- -- --             --         ∷L
-- -- -- --             --         (bindingS
-- -- -- --             --          (BS-let
-- -- -- --             --           (transp {λ i → ℓ-zero}
-- -- -- --             --            (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --            (AST.ice nothing 9 Bool))
-- -- -- --             --           {_} (lit false))
-- -- -- --             --          ∷L []L))
-- -- -- --             --        (lit true))))))
-- -- -- --             --   ∷L
-- -- -- --             --   (nonBindingS
-- -- -- --             --    (stmntNBS
-- -- -- --             --     (NBS-deposit! (AST.pId 2 {_}) {_}
-- -- -- --             --      (lit 2)))
-- -- -- --             --    ∷L
-- -- -- --             --    (nonBindingS
-- -- -- --             --     (stmntNBS
-- -- -- --             --      (NBS-withdraw! (AST.pId 2 {_}) {_}
-- -- -- --             --       (lit 3)))
-- -- -- --             --     ∷L
-- -- -- --             --     (bindingS
-- -- -- --             --      (BS-publish! (AST.pId 2 {_})
-- -- -- --             --       (psof 7 {_}) {_})
-- -- -- --             --      ∷L
-- -- -- --             --      (bindingS
-- -- -- --             --       (BS-let
-- -- -- --             --        (transp {λ i → ℓ-zero}
-- -- -- --             --         (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --         (transp {λ i → ℓ-zero}
-- -- -- --             --          (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --          (transp {λ i → ℓ-zero}
-- -- -- --             --           (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --           (transp {λ i → ℓ-zero}
-- -- -- --             --            (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --            (transp {λ i → ℓ-zero}
-- -- -- --             --             (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --             (transp {λ i → ℓ-zero}
-- -- -- --             --              (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
-- -- -- --             --              (AST.ice nothing 10 Bool)))))))
-- -- -- --             --        {_} (var (dsot 7 {_})))
-- -- -- --             --       ∷L []L))))))

-- -- -- --   zzz :
-- -- -- --     let q : ℕ × 𝟚 × 𝟚 × Unit
-- -- -- --         q = 3 , false , true , _
-- -- -- --     in (
-- -- -- --           set 6 ∶ Bool ≔ < true > ;
-- -- -- --           at 2 set 7 ∶ Bool ≔ < true > ;
-- -- -- --           at 1 set 8 ∶ Bool ≔ (
-- -- -- --               require! < false > ;'
-- -- -- --               -- publish! "B" ⟶ "y" ;
-- -- -- --               -- withdraw! "B" ⟵ < 3 > ;
-- -- -- --               -- deposit! "B" ⟶ < 2 > ;
-- -- -- --               set 9 ∶ Bool ≔ < false > ;b
-- -- -- --               < true >
-- -- -- --               );
-- -- -- --           deposit! 2 ⟶ < 2 > ;
-- -- -- --           withdraw! 2 ⟵ < 3 > ;
-- -- -- --           publish! 2 ⟶ 7 ;'        
-- -- -- --           set 10 ∶ Bool ≔ v 7 ) ≡ param-sub-test q

-- -- -- --   zzz = refl

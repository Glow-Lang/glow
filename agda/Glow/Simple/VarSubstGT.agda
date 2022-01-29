
{-# OPTIONS --cubical  #-}
module Glow.Simple.VarSubstGT where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool hiding (if_then_else_)  renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.Simple.AST

open import Glow.DecEqMore

open import Glow.Simple.ContextMore

open import Cubical.HITs.Interval

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
            {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where

  -- prop-mode = one
  
  -- open AST Identifier prop-mode

  open PropMode one 

  -- open AST Identifier
    


  module SubstOne {ptps : List Identifier} where
  
    -- module AST* = AST Identifier builtIns one

    open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps []) 

    Subst$ : Context → Type₀
    Subst$ = Maybe ∘ Subst

    remSubst$ : ∀ Γ → Subst$ Γ → Context
    remSubst$ Γ nothing = Γ
    remSubst$ Γ (just x) = remSubst Γ x


    bindingMechanics'-Subst : {c : Context} → Subst c → (x : Stmnt c) → Maybe (Subst (bindingMechanics' c x))


    bindingMechanics'*$ : (c : Σ Context Subst$) → Statement* c → Σ Context Subst$
    bindingMechanics'*$ c x =
          (bindingMechanics' (fst c) x)
         , bindMaybe (snd c) λ x₁ → bindingMechanics'-Subst x₁ x 


    Statements*$ : Σ Context Subst$ → Type₀
    Statements*$ = Linked' {A = Statement*} bindingMechanics'*$ 


    bindingMechanics'-Subst r (AST.bindingS (AST.BS-let ce x)) = just (inr r)
    bindingMechanics'-Subst r (AST.bindingS (AST.BS-publish! p x@(AST.psof name {y}) {z})) with (ExistFirstBy-WitchIsAlso-remSubs-lemm {p = p} _ r y)
    ... | inl _ = just (publish-substlemma r p x z) 
    ... | inr _ = nothing   
      

    bindingMechanics'-Subst r (AST.nonBindingS x) = just r

    -- move as more general property to Glow.Linked' module
    mkStatements*$ : ∀ {Γ r} → Statements Γ → Statements*$ (Γ , r)
    mkStatements*$ []L = []L
    mkStatements*$ (h ∷L x) = h ∷L mkStatements*$  x


    fold*-lemma : ∀ {Γ : Context}
                    (stmnts₁ : Statements Γ) → ∀ (r) →                   
                   fst (foldLinked' (mkStatements*$ {_} {r} stmnts₁)) ≡ foldLinked' stmnts₁
    fold*-lemma []L r = refl
    fold*-lemma (h ∷L stmnts₁) _ = fold*-lemma  (stmnts₁) _ 
   

    -- TODO : remove unsafe pragma by stratification on nesting depth
    {-# TERMINATING #-}

    substOneStmnt : ∀ {Γ} → (r : Subst Γ) → Stmnt Γ → Stmnt (remSubst$ Γ (just r))

    substOneExpr : ∀ {Γ Τ} → (r : Subst Γ) → Expr Γ Τ → Expr (remSubst$ Γ (just r)) Τ

    substOneStmnts$ : ∀ {Γ} → (r : Subst$ Γ) → Statements*$ (Γ , r) → Statements (remSubst$ Γ r) 

    substOneStmnt$ : ∀ {Γ} → (r : Subst$ Γ) → Stmnt Γ → Stmnt (remSubst$ Γ r)

    substOneExpr$ : ∀ {Γ Τ} → (r : Subst$ Γ) → Expr Γ Τ → Expr (remSubst$ Γ r) Τ

    substOneStmnts-coh$ :  ∀ Γ → (r : Subst$ Γ) → (x : Stmnt Γ) → remSubst$ (fst (bindingMechanics'*$ (Γ , r) x))
                                                                    (snd (bindingMechanics'*$ (Γ , r) x))
                                                                    ≡ bindingMechanics' (remSubst$ Γ r) (substOneStmnt$ r x)




    evalVar' : ∀ (Γ) → ∀ {Τ} → ∀ nm → ⟨ IsDefinedSymbolOfTy Γ Τ nm ⟩ → (r : Subst Γ) → ⟨ IsDefinedSymbolOfTy (remSubst Γ r) Τ nm ⟩ ⊎ GTypeAgdaRep Τ 
    evalVar' (AST.con (x₁ ∷ entries₁) scope'') nm (inl (inl x)) (inl x₂) = inr (subst⁻ GTypeAgdaRep (proj₂ (proj₂ x)) x₂)
    evalVar' (AST.con (x₁ ∷ entries₁) scope'') nm (inl (inr x)) (inl x₂) = 
        inl (inl (ExistFirstBy-WitchIsAlso-FilterOut-lemma entries₁ (λ a x₃ y → proj₁ x (x₃ ∙ sym y)) (proj₂ x)))
    evalVar' (AST.con (x₁ ∷ entries₁) scope'') nm (inl (inl x)) (inr x₂) = 
         inl (inl (inl x))
    evalVar' (AST.con (x₁ ∷ entries₁) scope'') nm (inl (inr x)) (inr x₂) = 
      let z = evalVar' (AST.con (entries₁) scope'') nm (inl (proj₂ x)) x₂
      in sum-map (sum-map (inr ∘ (proj₁ x ,_)) λ x₃ → empty-elim (lower (proj₂ x₃))) (idfun _) z
    evalVar' Γ nm (inr (x , ())) r
  


    substOneStmnt r (AST.bindingS (AST.BS-let ce {y} x )) = 
       let x' = substOneExpr r x
       in (AST.bindingS (AST.BS-let ce {y} x' ))

    substOneStmnt {AST.con entries₁ nothing} r (AST.bindingS (AST.BS-publish! p (AST.psof name {w}) {y})) with (ExistFirstBy-WitchIsAlso-remSubs-lemm {p = p} _ r w)
    ... | inl w = (AST.bindingS (AST.BS-publish! p (AST.psof name {proj₂ w}) {y}))
    ... | inr x = 
        let vv = SubstMatch-Extract _ _ _ r w (proj₁ x)
        in (AST.bindingS (AST.BS-let (AST.ice nothing name (fst vv)) ((receivePublished (snd vv))))) 
   -- (publishVal (snd vv))
   
    
    substOneStmnt r (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! x))) =
                      (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! (substOneExpr r x))))
    substOneStmnt r (AST.nonBindingS (AST.stmntNBS (AST.NBS-deposit! x {z} x₁ ))) =
                     (AST.nonBindingS (AST.stmntNBS (AST.NBS-deposit! x {z} (substOneExpr r x₁))))
    substOneStmnt r (AST.nonBindingS (AST.stmntNBS (AST.NBS-withdraw! x {z} x₁))) =
                    (AST.nonBindingS (AST.stmntNBS (AST.NBS-withdraw! x {z} (substOneExpr r x₁))))
    
    substOneStmnt r (AST.nonBindingS (AST.exprNBS x)) = (AST.nonBindingS (AST.exprNBS (substOneExpr r x))) 

    substOneExpr r (AST.var (AST.dsot name {y})) = 
      sum-elim
        (λ y → (AST.var (AST.dsot name {y})))
        lit
        (evalVar' _ name y r)
    
    substOneExpr r (AST.body (AST.bodyR stmnts₁ expr₁)) = 
       let q = foldLinked' (mkStatements*$ {_} {just r} stmnts₁)
           e' = substOneExpr$ (subst (Subst$) ((fold*-lemma stmnts₁ (just r))) (snd q)) expr₁
           stmnts' = substOneStmnts$ (just r) (mkStatements*$ stmnts₁)
           p = map-Linked'-map-fold _ _
                         (λ ΓRec →  substOneStmnts-coh$ (fst ΓRec) (snd ΓRec)) (mkStatements*$ {_} {just r} stmnts₁)
 
        in stmnts' AST.;b subst⁻ (λ x₁ → Expr x₁ _) ( sym p ∙
            λ i → remSubst$ ((fold*-lemma stmnts₁ (just r)) i)
              (subst-filler Subst$ (fold*-lemma stmnts₁ (just r)) ( (snd (foldLinked' (mkStatements*$ stmnts₁)))) i)           
           ) e'
           
    substOneExpr r (AST.lit x) = (AST.lit x)
    substOneExpr r (AST.input x {y}) = (AST.input x {y})
    substOneExpr r (AST.receivePublished x {y}) = {!!} --(AST.publishVal x {y})
    substOneExpr r (AST.if x then x₁ else x₂) = (AST.if (substOneExpr r x) then (substOneExpr r x₁) else (substOneExpr r x₂))
    substOneExpr r (AST._$'_ f xs) = AST._$'_ f {!!} --(substOneArgs r xs)

    publish-subst-lemma : ∀ {Γ} → (r : Subst Γ) → ∀ p → ∀ nm → ∀ w → ∀ q →

                           remSubst$
                               (fst
                                (bindingMechanics'*$ (Γ , (just r))
                                 (bindingS (BS-publish! p (psof nm {w}) {q}))))
                               (snd
                                (bindingMechanics'*$ (Γ , (just r))
                                 (bindingS (BS-publish! p (psof nm {w}) {q}))))
                               ≡
                               bindingMechanics' (remSubst$ Γ (just r))
                               (substOneStmnt r (bindingS (BS-publish! p (psof nm {w}) {q})))
    publish-subst-lemma {AST.con ee@(AST.ice scope name type ∷ entries₁) nothing} r p nm w q with (ExistFirstBy-WitchIsAlso-remSubs-lemm {p = p} (ee) r w) 
    ... | inl x₁ = cong (λ xx → con xx nothing) (map-ExistingFirstBy-lemma3 {cs = ee} (λ y →
                                                                                          recMaybe Empty (λ p' → AST.pId-name _ _ _ p ≡ AST.pId-name _ _ _ p')
                                                                                          (AST.scope y)) w (λ _ → nothing) r (proj₁ x₁) (proj₂ x₁))
    ... | inr x₁ = cong (λ xx → con xx nothing) {!!}

  -- ? ∙ cong (λ xx → con xx nothing) (map-ExistingFirstBy-lemma2 {cs = entries₁} _ _ _ _ (proj₁ x₁))



    substOneStmnts$ r =
       map-Linked'-map _
          (λ {ΓRec} → substOneStmnt$ (snd ΓRec))
          λ ΓRec →  substOneStmnts-coh$ (fst ΓRec) (snd ΓRec)

    substOneStmnt$ nothing x = x
    substOneStmnt$ (just r) x = substOneStmnt r x

    substOneExpr$ nothing x = x
    substOneExpr$ (just r) x = substOneExpr r x

    substOneStmnts-coh$ Γ nothing x = refl
    substOneStmnts-coh$ Γ (just x₁) (AST.bindingS (AST.BS-let ce x)) = refl
    substOneStmnts-coh$ Γ (just r) (AST.bindingS (AST.BS-publish! p (AST.psof name {w}) {y})) = publish-subst-lemma {Γ} r p name w y
    substOneStmnts-coh$ Γ (just x₁) (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! x))) = refl
    substOneStmnts-coh$ Γ (just x₁) (AST.nonBindingS (AST.stmntNBS (AST.NBS-deposit! x x₂))) = refl
    substOneStmnts-coh$ Γ (just x₁) (AST.nonBindingS (AST.stmntNBS (AST.NBS-withdraw! x x₂))) = refl
    substOneStmnts-coh$ Γ (just x₁) (AST.nonBindingS (AST.exprNBS x)) = refl




--   module SubstAll {ptps : List Identifier} where

--     open AST.InteractionHead  {prop-mode = one} (AST.interactionHead ptps [])

--     {-# TERMINATING #-}
--     substAllStmnts : ∀ {Γ} → (r : Rec Γ) → Statements Γ → Statements (record Γ {entries = []}) 
--     substAllStmnts {AST.con [] scope''} r x = x
--     substAllStmnts {Γ@(AST.con (x₁ ∷ entries₁) scope'')} (y , r') x = 
--       substAllStmnts  r' (SubstOne.substOneStmnts (inl y) (SubstOne.mkStatements* x))

--     {-# TERMINATING #-}
--     substAllStmnt : ∀ {Γ} → (r : Rec Γ) → Stmnt Γ → Stmnt (record Γ {entries = []}) 
--     substAllStmnt {AST.con [] scope''} r x = x
--     substAllStmnt {Γ@(AST.con (x₁ ∷ entries₁) scope'')} (y , r') x = 
--       substAllStmnt  r' (SubstOne.substOneStmnt (inl y) x)

--     {-# TERMINATING #-}
--     substAllExpr : ∀ {Γ Τ} → (r : Rec Γ) → Expr Γ Τ → Expr (record Γ {entries = []}) Τ 
--     substAllExpr {AST.con [] scope''} r x = x
--     substAllExpr {Γ@(AST.con (x₁ ∷ entries₁) scope'')} (y , r') x = 
--       substAllExpr  r' (SubstOne.substOneExpr (inl y) x)

--     {-# TERMINATING #-}
--     evalPureExpr : ∀ {sc Τ} → (e : Expr (con [] sc) Τ) → ⟨ IsPureE e ⟩ → GTypeAgdaRep Τ 
--     evalPureExpr (AST.var (AST.dsot name {inr (x₁ , ())})) x
    
--     evalPureExpr (AST.body (AST.bodyR []L e)) x = evalPureExpr e (proj₂ x)
--     evalPureExpr (AST.body (AST.bodyR (AST.bindingS (AST.BS-let ce x₁) ∷L stmnts₁) e)) x =
--        let x₁' = evalPureExpr x₁ (proj₁ (proj₁ x))
--            e' = SubstOne.substOneExpr (inl x₁')
--                   ((AST.body (AST.bodyR (stmnts₁) e))) 
--        in dec-rec ⟨ IsPureE e' ⟩ {{proj₁ (snd (IsPureE e'))}}
--           (λ x₂ → evalPureExpr e' x₂)
--           subst-preserver-pure 

--       where
--         postulate subst-preserver-pure : _
        
--     evalPureExpr (AST.body (AST.bodyR (AST.bindingS (AST.BS-publish! p x₁) ∷L stmnts₁) expr₁)) ((() , x₃) , x₂)
       
--     evalPureExpr (AST.body (AST.bodyR (AST.nonBindingS _ ∷L stmnts₁) expr₁)) x =
--        evalPureExpr (AST.body (AST.bodyR (stmnts₁) expr₁)) ((proj₂ (proj₁ x)) , (proj₂ x))

--     evalPureExpr (AST.lit x₁) x = x₁
--     evalPureExpr (AST.if e then e₁ else e₂) x =
--        Cubical.Data.Bool.if evalPureExpr e (proj₁ x)
--           then evalPureExpr e₁ (proj₁ (proj₂ x))
--           else evalPureExpr e₂ (proj₂ (proj₂ x))

-- -- module Test where

-- --   open SubstOne {String} {{String-Discrete-postulated}} {"A" ∷ "B" ∷ []}

-- --   -- open AST.InteractionHead  {prop-mode = true} (AST.interactionHead ptps [])

-- --   -- test-stmnts : {!Statements ?!}
-- --   -- test-stmnts = {!!}


{-# OPTIONS --cubical  #-}
module Glow.Simple.VarSubstPrim where

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



  -- TODO : provide alternative implementation, substituting multiple variables in one pass, compare performance
  module SubstOne {ptps : List Identifier} where
  
    -- module AST* = AST Identifier builtIns one

    open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps []) 




    bindingMechanics'* : (c : Σ Context Subst) → Statement* c → Σ Context Subst

    bindingMechanics'-Subst : {c : Context} → Subst c → (x : Stmnt c) → Subst (bindingMechanics' c x)


    Statements* : Σ Context Subst → Type₀
    Statements* = Linked' {A = Statement*} bindingMechanics'* 


    bindingMechanics'-Subst r (AST.bindingS (AST.BS-let ce x)) = inr r
    bindingMechanics'-Subst r (AST.bindingS (AST.BS-publish! p x {z})) = publish-substlemma r p x z   
       


    bindingMechanics'-Subst r (AST.nonBindingS x) = r

    -- move as more general property to Glow.Linked' module
    mkStatements* : ∀ {Γ r} → Statements Γ → Statements* (Γ , r)
    mkStatements* []L = []L
    mkStatements* (h ∷L x) = h ∷L mkStatements*  x


    fold*-lemma : ∀ {Γ : Context}
                    (stmnts₁ : Statements Γ) → ∀ (r) →                   
                   fst (foldLinked' (mkStatements* {_} {r} stmnts₁)) ≡ foldLinked' stmnts₁
    fold*-lemma []L r = refl
    fold*-lemma (h ∷L stmnts₁) x = fold*-lemma  (stmnts₁) (bindingMechanics'-Subst x h) 


    evalVar' : ∀ (Γ) → ∀ {Τ} → ∀ nm → ⟨ IsDefinedSymbolOfTy Γ Τ nm ⟩ → (r : Subst Γ) → ⟨ IsDefinedSymbolOfTy (remSubst Γ r) Τ nm ⟩ ⊎ GTypeAgdaRep Τ 

    -- TODO : remove unsafe pragma by stratification on nesting depth
    {-# TERMINATING #-}
    substOneStmnts : ∀ {Γ} → (r : Subst Γ) → Statements* (Γ , r) → Statements (remSubst Γ r) 

    substOneStmnt : ∀ {Γ} → (r : Subst Γ) → Stmnt Γ → Stmnt (remSubst Γ r)

    substOneExpr : ∀ {Γ Τ} → (r : Subst Γ) → Expr Γ Τ → Expr (remSubst Γ r) Τ

    substOneArg : ∀ {Γ Τ} → (r : Subst Γ) → Arg Γ Τ → Arg (remSubst Γ r) Τ
    substOneArg r (AST.var-a (AST.dsot name {y})) =
      sum-elim
        (λ y → (AST.var-a (AST.dsot name {y})))
        lit-a
        (evalVar' _ name y r)

    substOneArg r (AST.lit-a x) = lit-a x


    substOneArgs : ∀ {Γ Τs} → (r : Subst Γ) → Args Γ Τs → Args (remSubst Γ r) Τs
    substOneArgs {Τs = []} r x = tt
    substOneArgs {Τs = x₁ ∷ []} r x = substOneArg r x 
    substOneArgs {Τs = x₁ ∷ x₂ ∷ Τs} r (x , x₃) = substOneArg r x , substOneArgs r x₃



    substOneStmnts-coh :  ∀ Γ → (r : Subst Γ) → (x : Stmnt Γ) →
                                                    remSubst (fst (bindingMechanics'* (Γ , r) x))
                                                      (snd (bindingMechanics'* (Γ , r) x))
                                                      ≡ bindingMechanics' (remSubst Γ r) (substOneStmnt r x)



    evalVar' (AST.con (x₁ ∷ entries₁) scope'') nm (inl (inl x)) (inl x₂) = inr (subst⁻ GTypeAgdaRep (proj₂ (proj₂ x)) x₂)
    evalVar' (AST.con (x₁ ∷ entries₁) scope'') nm (inl (inr x)) (inl x₂) =
       inl (inl (ExistFirstBy-WitchIsAlso-FilterOut-lemma entries₁ (λ a x₃ y → proj₁ x (x₃ ∙ sym y)) (proj₂ x)))
    evalVar' (AST.con (x₁ ∷ entries₁) scope'') nm (inl (inl x)) (inr x₂) = inl (inl (inl x))
    evalVar' (AST.con (x₁ ∷ entries₁) scope'') nm (inl (inr x)) (inr x₂) =
      let z = evalVar' (AST.con (entries₁) scope'') nm (inl (proj₂ x)) x₂
      in sum-map (sum-map (inr ∘ (proj₁ x ,_)) λ x₃ → empty-elim (lower (proj₂ x₃))) (idfun _) z
    evalVar' Γ nm (inr (x , ())) r
  
    bindingMechanics'* c x = 
       (bindingMechanics' (fst c) x) ,
         bindingMechanics'-Subst (snd c) x


    substOneStmnts r = 
       map-Linked'-map _
          (λ {ΓRec} → substOneStmnt (snd ΓRec))
          λ ΓRec →  substOneStmnts-coh (fst ΓRec) (snd ΓRec)


    substOneStmnt r (AST.bindingS (AST.BS-let ce {y} x )) =
       let x' = substOneExpr r x
       in (AST.bindingS (AST.BS-let ce {y} x' ))

    substOneStmnt r (AST.bindingS (AST.BS-publish! p (AST.psof name {w}) {y})) =
      sum-elim
         (λ w → (AST.bindingS (AST.BS-publish! p (AST.psof name {proj₂ w}) {y})))
         (λ _ → (AST.nonBindingS (AST.stmntNBS (AST.NBS-publishVal! p name {y}))))
        (ExistFirstBy-WitchIsAlso-remSubs-lemm {p = p} _ r w) 

   
    
    substOneStmnt r (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! x))) =
                      (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! (substOneExpr r x))))
    substOneStmnt r (AST.nonBindingS (AST.stmntNBS (AST.NBS-deposit! x {z} x₁ ))) =
                     (AST.nonBindingS (AST.stmntNBS (AST.NBS-deposit! x {z} (substOneExpr r x₁))))
    substOneStmnt r (AST.nonBindingS (AST.stmntNBS (AST.NBS-withdraw! x {z} x₁))) =
                    (AST.nonBindingS (AST.stmntNBS (AST.NBS-withdraw! x {z} (substOneExpr r x₁))))
    substOneStmnt r (AST.nonBindingS (AST.stmntNBS (AST.NBS-publishVal! x y {z}))) =
                    (AST.nonBindingS (AST.stmntNBS (AST.NBS-publishVal! x y {z})))
    
    substOneStmnt r (AST.nonBindingS (AST.exprNBS x)) = (AST.nonBindingS (AST.exprNBS (substOneExpr r x))) 

    substOneExpr r (AST.var (AST.dsot name {y})) =
      sum-elim
        (λ y → (AST.var (AST.dsot name {y})))
        lit
        (evalVar' _ name y r)
    
    substOneExpr r (AST.body (AST.bodyR stmnts₁ expr₁)) =
       let q = foldLinked' (mkStatements* {_} {r} stmnts₁)
           e' = substOneExpr (subst (Subst) (fold*-lemma stmnts₁ r) (snd q)) expr₁
           stmnts' = substOneStmnts r (mkStatements* stmnts₁)
           p = map-Linked'-map-fold _ _
                         (λ ΓRec →  substOneStmnts-coh (fst ΓRec) (snd ΓRec)) (mkStatements* {_} {r} stmnts₁)
 
        in stmnts' AST.;b subst⁻ (λ x₁ → Expr x₁ _) ( sym p ∙
            λ i → remSubst ((fold*-lemma stmnts₁ r) i) (subst-filler Subst (fold*-lemma stmnts₁ r) ((snd (foldLinked' (mkStatements* stmnts₁)))) i)           
           ) e'
    substOneExpr r (AST.lit x) = (AST.lit x)
    substOneExpr r (AST.input x {y}) = (AST.input x {y})
    substOneExpr r (AST.receivePublished x {y}) = (AST.receivePublished x {y})
    substOneExpr r (AST.if x then x₁ else x₂) = (AST.if (substOneExpr r x) then (substOneExpr r x₁) else (substOneExpr r x₂))
    substOneExpr r (AST._$'_ f xs) = AST._$'_ f (substOneArgs r xs)


    -- publish-subst-lemma : ∀ {Γ} → (r : Subst Γ) → ∀ p → ∀ nm → ∀ w → ∀ q → 
    --                        remSubst
    --                            (fst
    --                             (bindingMechanics'* (Γ , r)
    --                              (bindingS (BS-publish! p (psof nm {w}) {q}))))
    --                            (snd
    --                             (bindingMechanics'* (Γ , r)
    --                              (bindingS (BS-publish! p (psof nm {w}) {q}))))
    --                            ≡
    --                            bindingMechanics' (remSubst Γ r)
    --                            (substOneStmnt r (bindingS (BS-publish! p (psof nm {w}) {q})))
    -- publish-subst-lemma {AST.con entries₁ scope''} r p nm w q with (ExistFirstBy-WitchIsAlso-remSubs-lemm {p = p} (entries₁) r w) 
    -- ... | inl x₁ = cong (λ xx → con xx scope'') (map-ExistingFirstBy-lemma3 {cs = entries₁} _ _ _ _ (proj₁ x₁) (proj₂ x₁))
    -- ... | inr x₁ = cong (λ xx → con xx scope'') (map-ExistingFirstBy-lemma2 {cs = entries₁} _ _ _ _ (proj₁ x₁))
    
    substOneStmnts-coh Γ r (AST.bindingS (AST.BS-let ce x)) = refl
    substOneStmnts-coh Γ r (AST.bindingS (AST.BS-publish! p (AST.psof name {w}) {q})) = ?
      -- publish-subst-lemma {Γ} r p name w q
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! x))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-deposit! x x₁))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-withdraw! x x₁))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-publishVal! x x₁))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.exprNBS x)) = refl

    substOneStmnts-coh-list : ∀ {Γ} → (r : Subst Γ) → (ss : Statements Γ) → _
    substOneStmnts-coh-list {Γ} r stmnts₁ =
       map-Linked'-map-fold {fld' = bindingMechanics'} (λ v → remSubst (fst v) (snd v)) (λ {ΓRec} → substOneStmnt (snd ΓRec))
                         (λ ΓRec →  substOneStmnts-coh (fst ΓRec) (snd ΓRec)) (mkStatements* {_} {r} stmnts₁)

--   module SubstAll {ptps : List Identifier} where

--     open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps []) 


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

--     evalArgs : ∀ {Τs sc} → Args (con [] sc) Τs → argsV Τs
--     evalArgs {[]} x = tt
--     evalArgs {x₁ ∷ []} (AST.var-a (AST.dsot name {inr (x , ())}))
--     evalArgs {x₁ ∷ []} (AST.lit-a x) = x , _
--     evalArgs {x₁ ∷ x₂ ∷ Τs} (AST.var-a (AST.dsot name {inr (x , ())}) , x₃)
--     evalArgs {x₁ ∷ x₂ ∷ Τs} (AST.lit-a x , x₃) = x , evalArgs x₃
    


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
--     evalPureExpr (AST._$'_ f xs) x =
--        let z = BuiltIn'.impl (snd (BuiltIns'.getBi builtIns (AST.BI.bIndex f)))
--            q = appV z (transport (cong argsV (AST.BI.dm≡ f)) (evalArgs xs))
--        in (transport⁻ (cong GTypeAgdaRep (AST.BI.cdm≡ f)) q)
       
-- -- module Test where

-- --   open SubstOne {String} {{String-Discrete-postulated}} {"A" ∷ "B" ∷ []}

-- --   -- open AST.InteractionHead  {prop-mode = true} (AST.interactionHead ptps [])

-- --   -- test-stmnts : {!Statements ?!}
-- --   -- test-stmnts = {!!}

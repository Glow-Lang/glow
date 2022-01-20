
{-# OPTIONS --cubical  #-}
module Glow.Simple.VarSubst where

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

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}} where



  module AlwaysCanPrepend {ptps : List Identifier} (ce : AST.ContextEntry (AST.interactionHead ptps []) ) where

    open AST.InteractionHead {prop-mode = one} (AST.interactionHead ptps []) 


    
    -- TODO : remove unsafe pragma by stratification on nesting depth
    {-# TERMINATING #-}
    prependCtxStmnts : ∀ {Γ : _} → Statements Γ → Statements (prependContext ce Γ) 



    prependCtxStmnts = map-Linked'-map _ h  hh
      where



        h : {Γ : Context}
               → (b : Stmnt Γ) → Stmnt (prependContext ce Γ)


        h-expr : {Γ : Context} → ∀ {Τ}
               → (b : Expr Γ Τ) → Expr (prependContext ce Γ) Τ


        h  (bindingS x) = bindingS (BS-lemma x)
           where
                BS-lemma : {Γ : Context} →  BStmnt Γ -> BStmnt (prependContext ce Γ)
                BS-lemma (BS-let x {asn} y) = (BS-let x {asn} (h-expr y))  
                BS-lemma (BS-publish! p (psof name₁ {w}) {y}) = 
                  (BS-publish! p (psof name₁ {(ExistFirstBy-WitchIsAlso-preppend-lemma _ _ w)}) {y})


        h (nonBindingS x) = nonBindingS (z x)
           where

             zz : NBStmnt _ → NBStmnt _ 
             zz (NBS-require! x) = NBS-require! (h-expr x)
             zz (NBS-deposit! p {y} x) = NBS-deposit! p {y} (h-expr x)
             zz (NBS-withdraw! p {y} x) = NBS-withdraw! p {y} (h-expr x)

             z : NBStmnt+Expr _ → NBStmnt+Expr _
             z (stmntNBS x) =  stmntNBS (zz x)
             z (exprNBS x) = exprNBS (h-expr x)

        h-expr (var (dsot x {y})) = var (dsot x { (
            sum-elim (λ a → (inl ((ExistFirstBy-WitchIsAlso-preppend-lemma _ _ a))))
             -- TODO : figure it out -- (λ a → var (dsot x {transport (λ i → {!True (ExistFirstBy-WitchIsAlso-preppend-lemma ? ? (fromWitness y) i)!}) y}))
             (λ b → empty-elim (lower (proj₂ b)))
              y)})

              --(var (dsot name₁ {transport {!!} y }))
        h-expr (stmnts₁ AST.;b x) =
            prependCtxStmnts stmnts₁ AST.;b subst (λ x₁ → Expr x₁ _)
             -- TODO : improve evaluation performance by introducing specialized "subst"
             -- specialisation should be not only on Expr, but also on map-Linked'-map-fold
          (map-Linked'-map-fold ((prependContext ce)) _ _ stmnts₁ ) (h-expr x)
        h-expr (lit x) = (AST.lit x)
        h-expr (input msg {y}) = input msg {y}
        h-expr (if b then t else f) = if (h-expr b) then (h-expr t) else (h-expr f)

        postulate hh : (Γ : Context) (x : Stmnt Γ) →
                           prependContext ce (bindingMechanics' Γ x) ≡
                           bindingMechanics'
                           (prependContext ce Γ) (h x)
        -- hh _ (bindingS (BS-let _ _)) = refl 
        -- hh _ (AST.bindingS (AST.BS-publish! _ _)) = {!!}
        -- hh _ (nonBindingS _) = refl


  -- TODO : provide alternative implementation, substituting multiple variables in one pass, compare performance
  module SubstOne {ptps : List Identifier} where

    open AST.InteractionHead  {prop-mode = one} (AST.interactionHead ptps [])




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


    -- TODO : remove unsafe pragma by stratification on nesting depth
    {-# TERMINATING #-}
    substOneStmnts : ∀ {Γ} → (r : Subst Γ) → Statements* (Γ , r) → Statements (remSubst Γ r) 

    substOneStmnt : ∀ {Γ} → (r : Subst Γ) → Stmnt Γ → Stmnt (remSubst Γ r)

    substOneExpr : ∀ {Γ Τ} → (r : Subst Γ) → Expr Γ Τ → Expr (remSubst Γ r) Τ

    substOneStmnts-coh :  ∀ Γ → (r : Subst Γ) → (x : Stmnt Γ) →
                                                    remSubst (fst (bindingMechanics'* (Γ , r) x))
                                                      (snd (bindingMechanics'* (Γ , r) x))
                                                      ≡ bindingMechanics' (remSubst Γ r) (substOneStmnt r x)


    evalVar' : ∀ (Γ) → ∀ {Τ} → ∀ nm → ⟨ IsDefinedSymbolOfTy Γ Τ nm ⟩ → (r : Subst Γ) → ⟨ IsDefinedSymbolOfTy (remSubst Γ r) Τ nm ⟩ ⊎ GTypeAgdaRep Τ 
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
         (λ _ → blankStmnt)
        (ExistFirstBy-WitchIsAlso-remSubs-lemm {p = p} _ r w) 

   
    
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
    substOneExpr r (AST.if x then x₁ else x₂) = (AST.if (substOneExpr r x) then (substOneExpr r x₁) else (substOneExpr r x₂))


    publish-subst-lemma : ∀ {Γ} → (r : Subst Γ) → ∀ p → ∀ nm → ∀ w → ∀ q → 
                           remSubst
                               (fst
                                (bindingMechanics'* (Γ , r)
                                 (bindingS (BS-publish! p (psof nm {w}) {q}))))
                               (snd
                                (bindingMechanics'* (Γ , r)
                                 (bindingS (BS-publish! p (psof nm {w}) {q}))))
                               ≡
                               bindingMechanics' (remSubst Γ r)
                               (substOneStmnt r (bindingS (BS-publish! p (psof nm {w}) {q})))
    publish-subst-lemma {AST.con entries₁ scope''} r p nm w q with (ExistFirstBy-WitchIsAlso-remSubs-lemm {p = p} (entries₁) r w) 
    ... | inl x₁ = cong (λ xx → con xx scope'') (map-ExistingFirstBy-lemma3 {cs = entries₁} _ _ _ _ (proj₁ x₁) (proj₂ x₁))
    ... | inr x₁ = cong (λ xx → con xx scope'') (map-ExistingFirstBy-lemma2 {cs = entries₁} _ _ _ _ (proj₁ x₁))
    
    substOneStmnts-coh Γ r (AST.bindingS (AST.BS-let ce x)) = refl
    substOneStmnts-coh Γ r (AST.bindingS (AST.BS-publish! p (AST.psof name {w}) {q})) = publish-subst-lemma {Γ} r p name w q
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! x))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-deposit! x x₁))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-withdraw! x x₁))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.exprNBS x)) = refl


  module SubstAll {ptps : List Identifier} where

    open AST.InteractionHead  {prop-mode = one} (AST.interactionHead ptps [])

    {-# TERMINATING #-}
    substAllStmnts : ∀ {Γ} → (r : Rec Γ) → Statements Γ → Statements (record Γ {entries = []}) 
    substAllStmnts {AST.con [] scope''} r x = x
    substAllStmnts {Γ@(AST.con (x₁ ∷ entries₁) scope'')} (y , r') x = 
      substAllStmnts  r' (SubstOne.substOneStmnts (inl y) (SubstOne.mkStatements* x))

    {-# TERMINATING #-}
    substAllStmnt : ∀ {Γ} → (r : Rec Γ) → Stmnt Γ → Stmnt (record Γ {entries = []}) 
    substAllStmnt {AST.con [] scope''} r x = x
    substAllStmnt {Γ@(AST.con (x₁ ∷ entries₁) scope'')} (y , r') x = 
      substAllStmnt  r' (SubstOne.substOneStmnt (inl y) x)

    {-# TERMINATING #-}
    substAllExpr : ∀ {Γ Τ} → (r : Rec Γ) → Expr Γ Τ → Expr (record Γ {entries = []}) Τ 
    substAllExpr {AST.con [] scope''} r x = x
    substAllExpr {Γ@(AST.con (x₁ ∷ entries₁) scope'')} (y , r') x = 
      substAllExpr  r' (SubstOne.substOneExpr (inl y) x)



module Test where

  open SubstOne {String} {{String-Discrete-postulated}} {"A" ∷ "B" ∷ []}

  -- open AST.InteractionHead  {prop-mode = true} (AST.interactionHead ptps [])

  -- test-stmnts : {!Statements ?!}
  -- test-stmnts = {!!}

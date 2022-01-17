
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

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}} where


  -- open AST Identifier

  -- module AlwaysCanPrepend {ptps : List Identifier} (ce : AST.ContextEntry (AST.interactionHead ptps []) ) where

  --   open AST.InteractionHead {prop-mode = true} (AST.interactionHead ptps []) 


    
  --   -- TODO : remove unsafe pragma by stratification on nesting depth
  --   {-# TERMINATING #-}
  --   prependCtxStmnts : ∀ {Γ : _} → Statements Γ → Statements (prependContext ce Γ) 



  --   prependCtxStmnts = map-Linked'-map _ h hh
  --     where



  --       h : {Γ : Context}
  --              → (b : Stmnt Γ) → Stmnt (prependContext ce Γ)


  --       h-expr : {Γ : Context} → ∀ {Τ}
  --              → (b : Expr Γ Τ) → Expr (prependContext ce Γ) Τ


  --       h  (bindingS x) = bindingS (BS-lemma x)
  --          where
  --               BS-lemma : {Γ : Context} →  BStmnt Γ -> BStmnt (prependContext ce Γ)
  --               BS-lemma (BS-let x {asn} y) = (BS-let x {asn} (h-expr y))  
  --               BS-lemma (BS-publish! p (psof name₁ {w}) {y}) = 
  --                 (BS-publish! p (psof name₁ {(ExistFirstBy-WitchIsAlso-preppend-lemma _ _ w)}) {y})


  --       h (nonBindingS x) = nonBindingS (z x)
  --          where

  --            zz : NBStmnt _ → NBStmnt _ 
  --            zz (NBS-require! x) = NBS-require! (h-expr x)
  --            zz (NBS-deposit! p {y} x) = NBS-deposit! p {y} (h-expr x)
  --            zz (NBS-withdraw! p {y} x) = NBS-withdraw! p {y} (h-expr x)

  --            z : NBStmnt+Expr _ → NBStmnt+Expr _
  --            z (stmntNBS x) =  stmntNBS (zz x)
  --            z (exprNBS x) = exprNBS (h-expr x)

  --       h-expr (var (dsot x {y})) = var (dsot x { (
  --           sum-elim (λ a → (inl ((ExistFirstBy-WitchIsAlso-preppend-lemma _ _ a))))
  --            -- TODO : figure it out -- (λ a → var (dsot x {transport (λ i → {!True (ExistFirstBy-WitchIsAlso-preppend-lemma ? ? (fromWitness y) i)!}) y}))
  --            (λ b → empty-elim (lower (proj₂ b)))
  --             y)})

  --             --(var (dsot name₁ {transport {!!} y }))
  --       h-expr (stmnts₁ AST.;b x) =
  --           prependCtxStmnts stmnts₁ AST.;b subst (λ x₁ → Expr x₁ _)
  --            -- TODO : improve evaluation performance by introducing specialized "subst"
  --            -- specialisation should be not only on Expr, but also on map-Linked'-map-fold
  --         (map-Linked'-map-fold ((prependContext ce)) _ _ stmnts₁ ) (h-expr x)
  --       h-expr (lit x) = (AST.lit x)
  --       h-expr (input msg {y}) = input msg {y}
  --       h-expr (if b then t else f) = if (h-expr b) then (h-expr t) else (h-expr f)

  --       hh : (Γ : Context) (x : Stmnt Γ) →
  --                          prependContext ce (bindingMechanics' Γ x) ≡
  --                          bindingMechanics'
  --                          (prependContext ce Γ) (h x)
  --       hh _ (bindingS (BS-let _ _)) = refl 
  --       hh _ (AST.bindingS (AST.BS-publish! _ _)) = {!!}
  --       hh _ (nonBindingS _) = refl


  -- TODO : provide alternative implementation, substituting multiple variables in one pass, compare performance
  module SubstOne {ptps : List Identifier} where

    open AST.InteractionHead  {prop-mode = true} (AST.interactionHead ptps [])

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

    -- fold*-lemma' : ∀ {Γ : Context}
    --                 (stmnts₁ : Statements Γ) → ∀ (r) →
    --                  PathP (λ x → Subst (fold*-lemma stmnts₁ r x)) 
    --                  (snd (foldLinked' (mkStatements* {_} {r} stmnts₁)))
    --                  (
    --                 (subst (Subst) (fold*-lemma stmnts₁ r)
    --                            (snd (foldLinked' (mkStatements* stmnts₁)))))
    -- fold*-lemma' {AST.con (AST.ice scope name type ∷ entries₁) scope''} []L r = {!subst-filler _ _ _!}
    -- fold*-lemma' (h ∷L stmnts₁) r = {!!}


    -- TODO : remove unsafe pragma by stratification on nesting depth
    {-# TERMINATING #-}
    substOneStmnts : ∀ {Γ} → (r : Subst Γ) → Statements* (Γ , r) → Statements (remSubst Γ r) 

    substOneStmnt : ∀ {Γ} → (r : Subst Γ) → Stmnt Γ → Stmnt (remSubst Γ r)

    substOneExpr : ∀ {Γ Τ} → (r : Subst Γ) → Expr Γ Τ → Expr (remSubst Γ r) Τ

    substOneStmnts-coh :  ∀ Γ → (r : Subst Γ) → (x : Stmnt Γ) →
                                                    remSubst (fst (bindingMechanics'* (Γ , r) x))
                                                      (snd (bindingMechanics'* (Γ , r) x))
                                                      ≡ bindingMechanics' (remSubst Γ r) (substOneStmnt r x)

-- lit (subst GTypeAgdaRep {!proj₂ x!} x₂)
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
         (λ w → (AST.bindingS (AST.BS-publish! p (AST.psof name {w}) {y})))
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

    substOneStmnts-coh Γ r (AST.bindingS (AST.BS-let ce x)) = refl
    substOneStmnts-coh (AST.con (x ∷ entries₁) scope'') r (AST.bindingS (AST.BS-publish! p (AST.psof name {w}))) = {!!}
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-require! x))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-deposit! x x₁))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-withdraw! x x₁))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.exprNBS x)) = refl


  module SubstAll {ptps : List Identifier} where

    open AST.InteractionHead  {prop-mode = true} (AST.interactionHead ptps [])

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



  -- module SubstAll {ptps : List Identifier} where

  --   open AST.InteractionHead (AST.interactionHead ptps [])

  --   bindingMechanics'* : (c : Σ Context Rec) → Statement* c → Σ Context Rec

  --   bindingMechanics'-Rec : {c : Context} → Rec c → (x : Stmnt c) → Rec (bindingMechanics' c x)

  --   Statements* : Σ Context Rec → Type₀
  --   Statements* = Linked' {A = Statement*} bindingMechanics'* 


    
  --   -- TODO : remove unsafe pragma by stratification on nesting depth
  --   {-# TERMINATING #-}
  --   substAllStmnts : ∀ {Γ} → (r : Rec Γ) → Statements* (Γ , r) → Statements* (emptyContext , _) 

  --   substAllStmnt : ∀ {Γ} → Rec Γ → Stmnt Γ → Stmnt emptyContext 

  --   substAllExpr : ∀ {Γ Τ} → Rec Γ → Expr Γ Τ → Expr Γ Τ

  --   substAllStmnts-coh : ∀ Γ → (r : Rec Γ) → (x : Stmnt Γ) →
  --                                              (emptyContext , uniqueParams) ≡
  --                                              bindingMechanics'* (emptyContext , uniqueParams)
  --                                              (substAllStmnt r x)


  --   bindingMechanics'* c x =
  --      (bindingMechanics' (fst c) x) ,
  --        bindingMechanics'-Rec (snd c) x

  --   bindingMechanics'-Rec r (AST.bindingS (AST.BS-let ce x)) = {!!} , {!r!}
  --   bindingMechanics'-Rec r (AST.bindingS (AST.BS-publish! p x)) = {!!}
  --   bindingMechanics'-Rec r (AST.nonBindingS x) = r


  --   substAllStmnts r =
  --      map-Linked'-map _
  --         (λ {ΓRec} → substAllStmnt (snd ΓRec))
  --         λ ΓRec →  substAllStmnts-coh (fst ΓRec) (snd ΓRec)


  --   substAllStmnt = {!!}

  --   substAllExpr = {!!}

  --   substAllStmnts-coh =  {!!} --map-Linked'-map _ h hh


      -- where



      --   h : {Γ : Context}
      --          → (b : Stmnt Γ) → Stmnt (prependContext ce Γ)


      --   h-expr : {Γ : Context} → ∀ {Τ}
      --          → (b : Expr Γ Τ) → Expr (prependContext ce Γ) Τ


      --   h  (bindingS x) = bindingS (BS-lemma x)
      --      where
      --           BS-lemma : {Γ : Context} →  BStmnt Γ -> BStmnt (prependContext ce Γ)
      --           BS-lemma (BS-let x {asn} y) = (BS-let x {asn} (h-expr y))  
      --           BS-lemma (BS-publish! p (psof name₁ {w}) {y}) = 
      --             (BS-publish! p (psof name₁ {fromWitness (ExistFirstBy-WitchIsAlso-preppend-lemma _ _  (toWitness w))}) {y})


      --   h (nonBindingS x) = nonBindingS (z x)
      --      where

      --        zz : NBStmnt _ → NBStmnt _ 
      --        zz (NBS-require! x) = NBS-require! (h-expr x)
      --        zz (NBS-deposit! p {y} x) = NBS-deposit! p {y} (h-expr x)
      --        zz (NBS-withdraw! p {y} x) = NBS-withdraw! p {y} (h-expr x)

      --        z : NBStmnt+Expr _ → NBStmnt+Expr _
      --        z (stmntNBS x) =  stmntNBS (zz x)
      --        z (exprNBS x) = exprNBS (h-expr x)

      --   h-expr (var (dsot x {y})) = var (dsot x {fromWitness (
      --       sum-elim (λ a → (inl ((ExistFirstBy-WitchIsAlso-preppend-lemma _ _ a))))
      --        -- TODO : figure it out -- (λ a → var (dsot x {transport (λ i → {!True (ExistFirstBy-WitchIsAlso-preppend-lemma ? ? (fromWitness y) i)!}) y}))
      --        (λ b → empty-elim (lower (proj₂ b)))
      --         (toWitness y))})

      --         --(var (dsot name₁ {transport {!!} y }))
      --   h-expr (stmnts₁ AST.;b x) =
      --       prependCtxStmnts stmnts₁ AST.;b subst (λ x₁ → Expr x₁ _)
      --        -- TODO : improve evaluation performance by introducing specialized "subst"
      --        -- specialisation should be not only on Expr, but also on map-Linked'-map-fold
      --     (map-Linked'-map-fold ((prependContext ce)) _ _ stmnts₁ ) (h-expr x)
      --   h-expr (lit x) = (AST.lit x)

      --   hh : (Γ : Context) (x : Stmnt Γ) →
      --      prependContext ce (bindingMechanics' Γ x) ≡
      --      bindingMechanics'
      --      (prependContext ce Γ) (h x)
      --   hh _ (bindingS (BS-let _ _)) = refl 
      --   hh _ (AST.bindingS (AST.BS-publish! _ _)) = {!!}
      --   hh _ (nonBindingS _) = refl





-- -- -- module _ (ptps : List IdentifierTy) where

-- -- --   ih = interactionHead ptps [] 

-- -- --   open InteractionHead ih



-- -- --   substVar : (Γ : Context) → (ce : ContextEntry) →
-- -- --                  Statements (prependContext ce Γ) → GTypeAgdaRep (ce .type) → Statements Γ
-- -- --   substVar Γ ce x x₁ = map-Linked'-map {!prependContext ce!} {!!} {!!} x

  
-- -- -- module ExprEval  (ptps : List IdentifierTy) where

-- -- --   ih = interactionHead ptps [] 

-- -- --   open InteractionHead ih

-- -- --   evalStmnts : (ce : ContextEntry) → (vv : GTypeAgdaRep (ce .type)) → ∀ scp → Statements (InteractionHead.con [ ce ] scp) → Statements emptyContext
-- -- --   evalStmnts = {!!}


-- -- --   evalExpr : (ce : ContextEntry) → (vv : GTypeAgdaRep (ce .type)) → ∀ scp → ∀ Τ → Expr (InteractionHead.con [ ce ] scp) Τ  → Expr emptyContext Τ
-- -- --   evalExpr ce vv scp Τ (v name₁) = lit {!!}
-- -- --   evalExpr ce vv scp Τ ([]L ;b expr₁) = evalExpr ce vv scp Τ expr₁
  
-- -- --   evalExpr ce vv scp Τ (bindingS (BS-let ce₁ {y} x) ; stmnts₁ ;b expr₁) =
-- -- --     let x' = evalExpr ce vv (con [ ce ] (narrowScope (con [ ce ] scp) (scope ce₁) y) .scope') (ce₁ .type) x
-- -- --     in {!x'!}
-- -- --   evalExpr ce vv scp Τ (bindingS (BS-publish! p x) ; stmnts₁ ;b expr₁) = {!!}
  
-- -- --   evalExpr ce vv scp Τ (nonBindingS _ ; stmnts₁ ;b expr₁) = evalExpr ce vv scp Τ (stmnts₁ ;b expr₁)
    
-- -- --   evalExpr ce vv scp Τ (lit x) = lit x



-- -- module ExprEval  (ptps : List IdentifierTy) where

-- --   ih = interactionHead ptps [] 

-- --   open InteractionHead ih

  



-- --   {-# TERMINATING #-}
-- --   sbstVarStmnts : (Γ : Context) → (ce : ContextEntry) → AType ce →
-- --                              Statements (addToContext Γ ce) → Statements Γ

-- --   sbstVarStmnt : (ce : ContextEntry) → AType ce → {Γ : Context}  →
-- --                              Stmnt (addToContext Γ ce) → Stmnt Γ

-- --   sbstVarNBStmnt : (ce : ContextEntry) → AType ce → {Γ : Context}  →
-- --                              NBStmnt+Expr (addToContext Γ ce) → NBStmnt+Expr Γ

-- --   sbstVarBStmnt : (ce : ContextEntry) → AType ce → {Γ : Context}  →
-- --                              BStmnt (addToContext Γ ce) → BStmnt Γ

-- --   sbstVarExpr : (ce : ContextEntry) → AType ce → {Γ : Context}  → ∀ Τ → 
-- --                               Expr (addToContext Γ ce) Τ → Expr Γ Τ


-- --   sbstVarStmnt-coh : (ce : ContextEntry) → (vv : AType ce) → {Γ : Context} {x : Stmnt (addToContext Γ ce)} →
-- --                             bindingMechanics' (addToContext Γ ce) x
-- --                             ≡ addToContext (bindingMechanics'  Γ (sbstVarStmnt ce vv x)) ce


-- --   sbstVarStmnts-coh : (ce : ContextEntry) → (vv : AType ce) → {Γ : Context} (x : Statements (addToContext Γ ce)) →
-- --                             foldLinked' x
-- --                             ≡
-- --                             addToContext (foldLinked' (sbstVarStmnts Γ ce vv x)) ce


-- --   sbstVarStmnt ce vv {Γ} (bindingS x) = bindingS (sbstVarBStmnt ce vv {Γ} x)
-- --   sbstVarStmnt ce vv {Γ} (nonBindingS x) = nonBindingS (sbstVarNBStmnt ce vv {Γ} x)
    

  
-- --   sbstVarStmnts Γ ce vv = 
-- --      map-Linked'-map-bck
-- --         (λ x → addToContext x ce)
-- --         (sbstVarStmnt ce vv) (sbstVarStmnt-coh ce vv)


-- --   sbstVarNBStmnt ce vv {Γ} (stmntNBS (NBS-require! x)) =  stmntNBS (NBS-require! (sbstVarExpr ce vv {Γ} _ x))
-- --   sbstVarNBStmnt ce vv {Γ} (stmntNBS (NBS-deposit! x {y} x₁)) = stmntNBS (NBS-deposit! x {y} (sbstVarExpr ce vv {Γ} _ x₁))
-- --   sbstVarNBStmnt ce vv {Γ} (stmntNBS (NBS-withdraw! x {y} x₁)) = stmntNBS (NBS-withdraw! x {y} (sbstVarExpr ce vv {Γ} _ x₁))
  
-- --   sbstVarNBStmnt ce vv {Γ} (exprNBS x) = exprNBS (sbstVarExpr ce vv {Γ} _ x)

-- --   sbstVarBStmnt ce vv {Γ} (BS-let ce₁ {asn} x) = BS-let ce₁ {asn} (sbstVarExpr ce vv _ x) 
-- --   sbstVarBStmnt ce vv {Γ} (BS-publish! p (psof name₁ {z}) {y}) = {!!}
-- --                                -- BS-publish! p (sbstVarPrivateSymbolOf ce vv p x ) {y}

-- --   sbstVarExpr ce vv {Γ} Τ (var x) = {!!}
  
-- --   sbstVarExpr ce vv {Γ} Τ (stmnts₁ ;b expr₁) = 
-- --       sbstVarStmnts Γ ce vv stmnts₁
-- --             ;b sbstVarExpr ce vv Τ (subst (λ x → Expr x Τ) (sbstVarStmnts-coh ce vv stmnts₁) expr₁) 
         
         
-- --   sbstVarExpr ce vv {Γ} Τ (lit x) = lit x


-- --   sbstVarStmnt-coh ce vv {x = bindingS (BS-let ce₁ x)} = {!refl!}
-- --   sbstVarStmnt-coh ce vv {x = bindingS (BS-publish! p x)} = {!!}
-- --   sbstVarStmnt-coh ce vv {x = nonBindingS x} = refl

-- --   sbstVarStmnts-coh ce vv =
-- --     map-Linked'-map-fold-bck
-- --         (λ x → addToContext x ce)
-- --         (sbstVarStmnt ce vv) (sbstVarStmnt-coh ce vv)










-- -- -- module EvalFwd (ptps : List IdentifierTy) where

-- -- --   ih = interactionHead ptps [] 

-- -- --   open InteractionHead ih

   

-- -- --   preppend-narrow-comm : (Γ : Context) → (ce : ContextEntry) → ∀ scp → ∀ narrowOk → ∀  narrowOk' → 
-- -- --                                    prependContext ce (narrow Γ scp narrowOk) ≡
-- -- --                                         narrow (prependContext ce Γ) scp narrowOk'
-- -- --   preppend-narrow-comm = {!!}

-- -- --   {-# TERMINATING #-}
-- -- --   prependContextStmnts : (Γ : Context) → (ce : ContextEntry) →
-- -- --                              Statements Γ → Statements (prependContext ce Γ)

-- -- --   prependContextStmnt : (ce : ContextEntry) → {Γ : Context}  →
-- -- --                              Stmnt Γ → Stmnt (prependContext ce Γ)

-- -- --   prependContextNBStmnt : (ce : ContextEntry) → {Γ : Context}  →
-- -- --                              NBStmnt+Expr Γ → NBStmnt+Expr (prependContext ce Γ)

-- -- --   prependContextBStmnt : (ce : ContextEntry) → {Γ : Context}  →
-- -- --                              BStmnt Γ → BStmnt (prependContext ce Γ)

-- -- --   prependContextExpr : (ce : ContextEntry) → {Γ : Context}  → ∀ Τ → 
-- -- --                               Expr Γ Τ → Expr (prependContext ce Γ) Τ

-- -- --   prependContextPrivateSymbolOf : (ce : ContextEntry) → {Γ : Context}  → ∀ p → 
-- -- --                               PrivateSymbolOf Γ p → PrivateSymbolOf (prependContext ce Γ) p

-- -- --   prependContextIsDefinedSymbolOfTy : (ce : ContextEntry) → {Γ : Context}  → ∀ {Τ} → ∀ s → 
-- -- --                               ⟨ IsDefinedSymbolOfTy Γ Τ s ⟩ → ⟨ IsDefinedSymbolOfTy (prependContext ce Γ) Τ s ⟩ 


-- -- --   postulate prependContextStmnt-coh : (ce : ContextEntry) {Γ : Context} {x : Stmnt Γ} →
-- -- --                             prependContext ce (bindingMechanics'  Γ x)
-- -- --                             ≡
-- -- --                             bindingMechanics' (prependContext ce Γ) (prependContextStmnt ce x)


-- -- --   prependContextStmnts-coh : (ce : ContextEntry) {Γ : Context} (x : Statements Γ) →
-- -- --                             prependContext ce (foldLinked' x)
-- -- --                             ≡
-- -- --                             foldLinked' (prependContextStmnts Γ ce x)


-- -- --   prependContextStmnt ce {Γ} (bindingS x) = bindingS (prependContextBStmnt ce {Γ} x)
-- -- --   prependContextStmnt ce {Γ} (nonBindingS x) = nonBindingS (prependContextNBStmnt ce {Γ} x)

-- -- --   prependContextPrivateSymbolOf ce {con ents scope''} p x =
-- -- --     psof (x .name) {  subst (λ fbe → fst
-- -- --                               (Bool→Type'
-- -- --                                (recMaybe false
-- -- --                                 (λ y →
-- -- --                                    recMaybe false (λ p' → primStringEquality (name p) (name p'))
-- -- --                                    (scope y))
-- -- --                                 fbe ))) (findBy-preppend  _ ents ce ((lemma-mb-rec _ (x .isDefinedSymbolOf)))) (x .isDefinedSymbolOf)}

-- -- --   prependContextIsDefinedSymbolOfTy ce {con ents scope''} {Τ} s x =
-- -- --     subst (λ v → fst (Bool→Type'
-- -- --        (recMaybe false
-- -- --         (λ y →
-- -- --            (con ents scope'' InteractionHead.canAccessTest scope'') (scope y)
-- -- --            and GTy== (type y) Τ)
-- -- --         v))) (findBy-preppend  _ ents ce ((lemma-mb-rec _ x))) x

-- -- --   prependContextStmnts Γ ce =
-- -- --      map-Linked'-map
-- -- --         (prependContext ce)
-- -- --         (prependContextStmnt ce) (prependContextStmnt-coh ce)


-- -- --   prependContextNBStmnt ce {Γ} (stmntNBS (NBS-require! x)) =  stmntNBS (NBS-require! (prependContextExpr ce {Γ} _ x))
-- -- --   prependContextNBStmnt ce {Γ} (stmntNBS (NBS-deposit! x {y} x₁)) = stmntNBS (NBS-deposit! x {y} (prependContextExpr ce {Γ} _ x₁))
-- -- --   prependContextNBStmnt ce {Γ} (stmntNBS (NBS-withdraw! x {y} x₁)) = stmntNBS (NBS-withdraw! x {y} (prependContextExpr ce {Γ} _ x₁))
  
-- -- --   prependContextNBStmnt ce {Γ} (exprNBS x) = exprNBS (prependContextExpr ce {Γ} _ x)

-- -- --   prependContextBStmnt ce {Γ} (BS-let ce₁ {asn} x) =
-- -- --                                let (asn' , x') = maybe-elim
-- -- --                                            {B = λ scope* →
-- -- --                                                 Σ ⟨ AllowedScopeNarrowing Γ scope* ⟩ (λ asn → Expr (narrow Γ scope* asn) (type ce₁))
-- -- --                                                       → Σ ⟨ AllowedScopeNarrowing (prependContext ce Γ) scope* ⟩
-- -- --                                                              (λ asn → Expr (narrow (prependContext ce Γ) scope* asn) (type ce₁))}
-- -- --                                            (λ x → tt* , subst (λ y → Expr y (type ce₁)) (preppend-narrow-comm Γ _ _ _ _) (prependContextExpr ce _ (snd x)))
-- -- --                                            (λ _ x → fst x , subst (λ y → Expr y (type ce₁)) (preppend-narrow-comm Γ _ _ (fst x) (fst x)) (prependContextExpr ce _ (snd x)) )
-- -- --                                            (ce₁ .scope) (asn , x)
-- -- --                                in BS-let ce₁ {asn'} x'
-- -- --   prependContextBStmnt ce {Γ} (BS-publish! p x {y}) =
-- -- --                                BS-publish! p (prependContextPrivateSymbolOf ce p x ) {y}

-- -- --   prependContextExpr ce {Γ} Τ (var x) = var (dsot (x .name) {prependContextIsDefinedSymbolOfTy ce {Γ} {Τ} (x .name) (x .isDefinedSymbolOfTy)})
-- -- --   prependContextExpr ce {Γ} Τ (stmnts₁ ;b expr₁) =
-- -- --       let expr* = prependContextExpr ce Τ expr₁
-- -- --       in prependContextStmnts Γ ce stmnts₁ ;b
-- -- --            subst (λ y → Expr y Τ) (prependContextStmnts-coh ce stmnts₁) expr*
         
-- -- --   prependContextExpr ce {Γ} Τ (lit x) = lit x


-- -- --   -- prependContextStmnt-coh = {!!}

-- -- --   prependContextStmnts-coh ce = map-Linked'-map-fold _ _ _

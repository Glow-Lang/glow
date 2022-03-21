
{-# OPTIONS --cubical  #-}
module Glow.Simple.VarSubst where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sigma renaming (_×_ to _Σ×_)
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool hiding (if_then_else_)  renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )
open import Cubical.Data.Unit

open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.Simple.AST

open import Glow.DecEqMore

open import Glow.Simple.ContextMore

open import Glow.Simple.Postulates

open import Glow.ListDecProps


open import Cubical.HITs.Interval

open import Cubical.Categories.Category
open import Cubical.Categories.Functor
open import Cubical.Categories.Instances.Categories
open import Cubical.Categories.Constructions.Slice
open import Cubical.HITs.S1

-- open import Glow.CategoriesMore
-- module functorIsNotASet where
--   isNotSet : ∀ {ℓ} → (A : Type ℓ) → Type ℓ
--   isNotSet A = Σ A λ a → Σ ((a ≡ a) Σ× (a ≡ a)) λ x → fst x ≡ snd x → Empty

--   isNotSet-S1 : isNotSet S¹
--   fst isNotSet-S1 = base
--   fst (snd isNotSet-S1) = refl , loop
--   snd (snd isNotSet-S1) x = znots (injPos (cong winding x))


--   TERMINAL : ∀ {ℓ} → Type ℓ → Category ℓ ℓ-zero
--   Category.ob (TERMINAL A) = A
--   Category.Hom[_,_] (TERMINAL A) _ _ = Unit
--   Category.id (TERMINAL A) = tt
--   Category._⋆_ (TERMINAL A) _ _ = tt
--   Category.⋆IdL (TERMINAL A) _ = refl
--   Category.⋆IdR (TERMINAL A) _ = refl
--   Category.⋆Assoc (TERMINAL A) _ _ _ = refl
--   Category.isSetHom (TERMINAL A) = isSetUnit


--   PointFunctor : ∀ {ℓ} → (A : Type ℓ) → A → (Functor (TERMINAL A) (TERMINAL A))
--   Functor.F-ob (PointFunctor A x) = const x
--   Functor.F-hom (PointFunctor A x) = idfun _
--   Functor.F-id (PointFunctor A x) = refl
--   Functor.F-seq (PointFunctor A x) _ _ = refl

--   isNotSet-Functor : ∀ {ℓ} → (A : Type ℓ) → (isNotSet A) → isNotSet (Functor (TERMINAL A) (TERMINAL A))
--   fst (isNotSet-Functor A x) = PointFunctor _ (fst x)
--   Functor.F-ob (fst (fst (snd (isNotSet-Functor A x))) i) _ = fst (fst (snd x)) i
--   Functor.F-hom (fst (fst (snd (isNotSet-Functor A x))) i) = idfun _
--   Functor.F-id (fst (fst (snd (isNotSet-Functor A x))) i) = refl
--   Functor.F-seq (fst (fst (snd (isNotSet-Functor A x))) i) _ _ = refl
--   Functor.F-ob (snd (fst (snd (isNotSet-Functor A x))) i) _ = snd (fst (snd x)) i
--   Functor.F-hom (snd (fst (snd (isNotSet-Functor A x))) i) = idfun _
--   Functor.F-id (snd (fst (snd (isNotSet-Functor A x))) i) = refl
--   Functor.F-seq (snd (fst (snd (isNotSet-Functor A x))) i) _ _ = refl
--   snd (snd (isNotSet-Functor A x)) x₁ = snd (snd x) λ i i₁ → Functor.F-ob ( x₁ i i₁ ) (fst x)

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
            {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where

  -- prop-mode = one
  
  -- open AST Identifier prop-mode

  open PropMode one 

  -- open AST Identifier




  module AlwaysCanPrepend {ptps : List (Identifier × ParticipantModality)} {uniquePtps : _}
                          (ce : AST.ContextEntry (AST.interactionHead ptps [] {_} {uniquePtps}) ) where

    open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps [] {_} {uniquePtps}) 


    
    -- TODO : remove unsafe pragma by stratification on nesting depth
    --         OR by introducing specialized subst in h-expr (stmnts₁ AST.;b x) 
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
             zz (NBS-deposit! p x) = NBS-deposit! p (h-expr x)
             zz (NBS-withdraw! p x) = NBS-withdraw! p (h-expr x)
             zz (NBS-publishVal! x x₁) = (NBS-publishVal! x x₁)

             z : NBStmnt+Expr _ → NBStmnt+Expr _
             z (stmntNBS x {z}) =  stmntNBS (zz x) {z}
             z (exprNBS x) = exprNBS (h-expr x)


        substOneArg : ∀ {Γ Τ} → Arg Γ Τ → Arg (prependContext ce Γ) Τ
        substOneArg (AST.var-a (AST.dsot x {y})) = 
           var-a (dsot x { (
            sum-elim (λ a → (inl ((ExistFirstBy-WitchIsAlso-preppend-lemma _ _ a))))
             (λ b → empty-elim (lower (proj₂ b)))
              y)})

        substOneArg (AST.lit-a x) = lit-a x


        substOneArgs : ∀ {Γ Τs}  → Args Γ Τs → Args (prependContext ce Γ) Τs
        substOneArgs {Τs = []} x = tt
        substOneArgs {Τs = x₁ ∷ []} x = substOneArg x 
        substOneArgs {Τs = x₁ ∷ x₂ ∷ Τs} (x , x₃) = substOneArg x , substOneArgs x₃


        h-expr (var (dsot x {y})) = var (dsot x { (
            sum-elim (λ a → (inl ((ExistFirstBy-WitchIsAlso-preppend-lemma _ _ a))))
             
             (λ b → empty-elim (lower (proj₂ b)))
              y)})


        h-expr (stmnts₁ AST.;b x) =
            prependCtxStmnts stmnts₁ AST.;b subst (λ x₁ → Expr x₁ _)
             -- TODO : improve evaluation performance by introducing specialized "subst"
             -- specialisation should be not only on Expr, but also on map-Linked'-map-fold
          (map-Linked'-map-fold ((prependContext ce)) _ _ stmnts₁ ) (h-expr x)
        h-expr (lit x) = (AST.lit x)
        h-expr (input msg {y}) = input msg {y}
        h-expr (receivePublished p {y}) = receivePublished p {y}
        h-expr (if b then t else f) = if (h-expr b) then (h-expr t) else (h-expr f)
        h-expr (AST._$'_ f xs) = AST._$'_ f (substOneArgs xs)
        h-expr (AST.sign q {y} {z}) = AST.sign (substOneArg q) {y} {z}


        postulate hh : (Γ : Context) (x : Stmnt Γ) →
                           prependContext ce (bindingMechanics' Γ x) ≡
                           bindingMechanics'
                           (prependContext ce Γ) (h x)
        -- hh _ (bindingS (BS-let _ _)) = refl 
        -- hh _ (AST.bindingS (AST.BS-publish! _ _)) = {!!}
        -- hh _ (nonBindingS _) = refl


  -- TODO : provide alternative implementation, substituting multiple variables in one pass, compare performance
  module SubstOne {ptps : List (Identifier × ParticipantModality)} {uniquePtps : _} where
  
    -- module AST* = AST Identifier builtIns one

    open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps [] {_} {uniquePtps}) 


    SUBST : Category ℓ-zero ℓ-zero
    SUBST = FreeCategory' remSubst isSet-Context isSet-Subst


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

    fromStatements* : ∀ {Γ r} → Statements* (Γ , r) → Statements Γ
    fromStatements* []L = []L
    fromStatements* (h ∷L x) = h ∷L fromStatements*  x


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



    evalVar' (AST.con (x₁ ∷ entries₁) scope'') nm (inl (inl x)) (inl x₂) =
       inr (subst-GTypeAgdaRep (sym (proj₂ (proj₂ x))) x₂)
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
         (λ _ → (AST.nonBindingS (AST.stmntNBS (AST.NBS-publishVal! p name) {y})))
        (ExistFirstBy-WitchIsAlso-remSubs-lemm {p = p} _ r w) 

   
    substOneStmnt r (AST.nonBindingS (stmntNBS q {z})) = (AST.nonBindingS (stmntNBS (h q) {z}))
       where
         h : NBStmnt _ → _
         h (AST.NBS-require! x) =
             (AST.NBS-require! (substOneExpr r x))
         h (AST.NBS-deposit! x x₁) =
             (AST.NBS-deposit! x (substOneExpr r x₁))
         h (AST.NBS-withdraw! x x₁) =
             (AST.NBS-withdraw! x (substOneExpr r x₁))         
         h (AST.NBS-publishVal! x x₁) =
             (AST.NBS-publishVal! x x₁)
    
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
    substOneExpr r (AST.receivePublished p {y}) = (AST.receivePublished p {y})
    substOneExpr r (AST.if x then x₁ else x₂) = (AST.if (substOneExpr r x) then (substOneExpr r x₁) else (substOneExpr r x₂))
    substOneExpr r (AST._$'_ f xs) = AST._$'_ f (substOneArgs r xs)
    substOneExpr r (AST.sign q {y} {z}) = (AST.sign (substOneArg r q) {y} {z})

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
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.stmntNBS (AST.NBS-publishVal! x x₁))) = refl
    substOneStmnts-coh Γ r (AST.nonBindingS (AST.exprNBS x)) = refl

    substOneStmnts-coh-list : ∀ {Γ} → (r : Subst Γ) → (ss : Statements Γ) → _
    substOneStmnts-coh-list {Γ} r stmnts₁ =
       map-Linked'-map-fold {fld' = bindingMechanics'} (λ v → remSubst (fst v) (snd v)) (λ {ΓRec} → substOneStmnt (snd ΓRec))
                         (λ ΓRec →  substOneStmnts-coh (fst ΓRec) (snd ΓRec)) (mkStatements* {_} {r} stmnts₁)

    substOneStmnts-coh-list' : ∀ {Γ} → (r : Subst Γ) → (ss : Statements* (Γ , r)) → _
    substOneStmnts-coh-list' {Γ} r stmnts₁ =
       map-Linked'-map-fold {fld' = bindingMechanics'} (λ v → remSubst (fst v) (snd v)) (λ {ΓRec} → substOneStmnt (snd ΓRec))
                         (λ ΓRec →  substOneStmnts-coh (fst ΓRec) (snd ΓRec)) (stmnts₁)


    SUBST* : Category ℓ-zero ℓ-zero
    SUBST* = FreeCategory' bindingMechanics'* (isSetΣ isSet-Context isSet-Subst) (isSet-Stmnt ∘ fst)


    subst-functor' : Functor _ _ -- SUBST* STMNTS
    subst-functor' = FreCatFunct.map-Linked'-map-functor {fld = bindingMechanics'*} {fld' = bindingMechanics'}
                       (isSetΣ isSet-Context isSet-Subst) (isSet-Stmnt ∘ fst {B = Subst}) isSet-Context isSet-Stmnt
                          (λ v → remSubst (fst v) (snd v))
                           (λ {ΓRec} → substOneStmnt {Γ = fst ΓRec} (snd ΓRec))
                         (λ ΓRec →  substOneStmnts-coh (fst ΓRec) (snd ΓRec))


    -- UR : UnnamedRel SUBST STMNTS
    -- UR = {!!}
      

  --   -- CoSliceCat : {ℓ ℓ' : Level} (C : Category ℓ ℓ') (c₁ : C .Category.ob) → Category (ℓ-max ℓ ℓ') ℓ'
  --   -- CoSliceCat C c = (SliceCat C c ^op) ^op

  --   -- postulate isSet-SliceOb-STMNTS : ∀ x → isSet (SliceOb STMNTS x)

  --   -- subst-functor : Functor SUBST (CAT ℓ-zero ℓ-zero)
  --   -- Functor.F-ob subst-functor c = (CoSliceCat STMNTS c ) , isSet-SliceOb-STMNTS _ 
  --   -- Functor.F-hom subst-functor {Γx} {Γy} c = w
  --   --   where
  --   --     w : Functor (fst (Functor.F-ob subst-functor Γx)) (fst (Functor.F-ob subst-functor Γy)) 
  --   --     Functor.F-ob w (sliceob S-arr₁) = sliceob ({!!} , {!!})
  --   --     Functor.F-hom w = {!!}
  --   --     Functor.F-id w = {!!}
  --   --     Functor.F-seq w = {!!}
  --   -- Functor.F-id subst-functor = {!!}
  --   -- Functor.F-seq subst-functor = {!!}
    
  --   -- subst*-functor-forget : Functor SUBST* STMNTS
  --   -- Functor.F-ob subst*-functor-forget = fst
  --   -- Functor.F-hom subst*-functor-forget x = {! (fst x)!} , {!!}
  --   -- Functor.F-id subst*-functor-forget = {!!}
  --   -- Functor.F-seq subst*-functor-forget = {!!}

  --   -- SubstUnderFunctor : Functor {!!} {!CAT!}
  --   -- SubstUnderFunctor = {!!}

  --   -- subst-functor : Functor SUBST* STMNTS
  --   -- Functor.F-ob subst-functor (Γ , r) = remSubst Γ r
  --   -- fst (Functor.F-hom subst-functor {x = (_ , r)} (l , _)) = substOneStmnts r l        
  --   -- snd (Functor.F-hom subst-functor {x = (_ , r)} (l , p)) = cong₂ remSubst (cong fst p) (cong snd p) ∙ substOneStmnts-coh-list' r l 
  --   -- Functor.F-id subst-functor = ΣPathP (refl , (sym (doubleCompPath-filler refl refl refl)))
  --   -- Functor.F-seq subst-functor (fst₁ , snd₁) g = ΣPathP ({!!}  , {!!})


  --   -- subst-functor : Functor SUBST STMNTS
  --   -- Functor.F-ob subst-functor = idfun _
  --   -- fst (Functor.F-hom subst-functor (fst₁ , _)) = qq fst₁
  --   --   where
  --   --    qq : Linked' remSubst _ → Linked' bindingMechanics' _
  --   --    qq x = {!x!}
       
  --   -- snd (Functor.F-hom subst-functor (fst₁ , snd₁)) = {!!}
  --   -- Functor.F-id subst-functor = {!!}
  --   -- Functor.F-seq subst-functor = {!!}

  module SubstAll {ptps : List (Identifier × ParticipantModality)} {uniquePtps : _} where

    open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps [] {_} {uniquePtps}) 


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

    evalPureArg : ∀ {sc Τ} → (e : Arg (con [] sc) Τ) → GTypeAgdaRep Τ 
    evalPureArg (AST.var-a (AST.dsot name {inr (x , ())}))
    evalPureArg (AST.lit-a x) = x


    evalArgs : ∀ {Τs sc} → Args (con [] sc) Τs → argsV Τs
    evalArgs {[]} x = tt
    evalArgs {x₁ ∷ []} x = (evalPureArg x) , _
    evalArgs {x₁ ∷ x₂ ∷ Τs} (x , x₃) = evalPureArg x , evalArgs x₃
    

    {-# TERMINATING #-}
    evalPureExpr : ∀ {sc Τ} → (e : Expr (con [] sc) Τ) → ⟨ IsPureE e ⟩ → GTypeAgdaRep Τ 
    evalPureExpr (AST.var (AST.dsot name {inr (x₁ , ())})) x
    
    evalPureExpr (AST.body (AST.bodyR []L e)) x = evalPureExpr e (proj₂ x)
    evalPureExpr (AST.body (AST.bodyR (AST.bindingS (AST.BS-let ce x₁) ∷L stmnts₁) e)) x =
       let x₁' = evalPureExpr x₁ (proj₁ (proj₁ x))
           e' = SubstOne.substOneExpr (inl x₁')
                  ((AST.body (AST.bodyR (stmnts₁) e))) 
       in dec-rec ⟨ IsPureE e' ⟩ {{proj₁ (snd (IsPureE e'))}}
          (λ x₂ → evalPureExpr e' x₂)
          subst-preserver-pure 

      where
        postulate subst-preserver-pure : _
        
    evalPureExpr (AST.body (AST.bodyR (AST.bindingS (AST.BS-publish! p x₁) ∷L stmnts₁) expr₁)) ((() , x₃) , x₂)
       
    evalPureExpr (AST.body (AST.bodyR (AST.nonBindingS _ ∷L stmnts₁) expr₁)) x =
       evalPureExpr (AST.body (AST.bodyR (stmnts₁) expr₁)) ((proj₂ (proj₁ x)) , (proj₂ x))

    
    evalPureExpr (AST.lit x₁) x = x₁
    evalPureExpr (AST.if e then e₁ else e₂) x =
       Cubical.Data.Bool.if evalPureExpr e (proj₁ x)
          then evalPureExpr e₁ (proj₁ (proj₂ x))
          else evalPureExpr e₂ (proj₂ (proj₂ x))
    evalPureExpr (AST._$'_ f xs) x =
       let z = BuiltIn'.impl (snd (BuiltIns'.getBi builtIns (AST.BI.bIndex f)))
           q = appV z (evalArgs xs) 
       in subst-GTypeAgdaRep (sym (AST.BI.cdm≡ f)) q
         --(transport⁻ (cong GTypeAgdaRep (AST.BI.cdm≡ f)) q)
    evalPureExpr (AST.var (AST.dsot name {inl ()})) tt
    evalPureExpr {sc = sc} (AST.sign q {z} {p}) w =
        subst-GTypeAgdaRep p (signPrim (AST.pId-nameHon _ _ _ (IsNotConsensus→Participant
           {con [] sc}
             z)) (evalPureArg q))


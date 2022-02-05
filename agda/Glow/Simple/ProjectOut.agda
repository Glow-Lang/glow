
{-# OPTIONS --cubical  #-}
module Glow.Simple.ProjectOut where

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

open import Glow.Simple.Postulates


open import Cubical.HITs.Interval


module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
            {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where

  -- prop-mode = one
  
  -- open AST Identifier prop-mode

  open PropMode one 

  -- open AST Identifier



  -- TODO : provide alternative implementation, substituting multiple variables in one pass, compare performance
  module ProjectOut {ptps : List Identifier} (pid : AST.ParticipantId' Identifier builtIns one {ptps} ) where
  
    -- module AST* = AST Identifier builtIns one

    open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps [])
                                                                      

    projectOutStmnts : ∀ {Γ} → Statements Γ → Statements Γ
    projectOutStmnts = {!!}

    projectOutExpr : ∀ {Γ Τ} → Expr Γ Τ → Expr Γ Τ
    projectOutExpr = {!!}



  -- module SubstAll {ptps : List Identifier} where

  --   open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps []) 


  --   {-# TERMINATING #-}
  --   substAllStmnts : ∀ {Γ} → (r : Rec Γ) → Statements Γ → Statements (record Γ {entries = []}) 
  --   substAllStmnts {AST.con [] scope''} r x = x
  --   substAllStmnts {Γ@(AST.con (x₁ ∷ entries₁) scope'')} (y , r') x = 
  --     substAllStmnts  r' (SubstOne.substOneStmnts (inl y) (SubstOne.mkStatements* x))

  --   {-# TERMINATING #-}
  --   substAllStmnt : ∀ {Γ} → (r : Rec Γ) → Stmnt Γ → Stmnt (record Γ {entries = []}) 
  --   substAllStmnt {AST.con [] scope''} r x = x
  --   substAllStmnt {Γ@(AST.con (x₁ ∷ entries₁) scope'')} (y , r') x = 
  --     substAllStmnt  r' (SubstOne.substOneStmnt (inl y) x)

  --   {-# TERMINATING #-}
  --   substAllExpr : ∀ {Γ Τ} → (r : Rec Γ) → Expr Γ Τ → Expr (record Γ {entries = []}) Τ 
  --   substAllExpr {AST.con [] scope''} r x = x
  --   substAllExpr {Γ@(AST.con (x₁ ∷ entries₁) scope'')} (y , r') x = 
  --     substAllExpr  r' (SubstOne.substOneExpr (inl y) x)

  --   evalPureArg : ∀ {sc Τ} → (e : Arg (con [] sc) Τ) → GTypeAgdaRep Τ 
  --   evalPureArg (AST.var-a (AST.dsot name {inr (x , ())}))
  --   evalPureArg (AST.lit-a x) = x


  --   evalArgs : ∀ {Τs sc} → Args (con [] sc) Τs → argsV Τs
  --   evalArgs {[]} x = tt
  --   evalArgs {x₁ ∷ []} x = (evalPureArg x) , _
  --   evalArgs {x₁ ∷ x₂ ∷ Τs} (x , x₃) = evalPureArg x , evalArgs x₃
    

  --   {-# TERMINATING #-}
  --   evalPureExpr : ∀ {sc Τ} → (e : Expr (con [] sc) Τ) → ⟨ IsPureE e ⟩ → GTypeAgdaRep Τ 
  --   evalPureExpr (AST.var (AST.dsot name {inr (x₁ , ())})) x
    
  --   evalPureExpr (AST.body (AST.bodyR []L e)) x = evalPureExpr e (proj₂ x)
  --   evalPureExpr (AST.body (AST.bodyR (AST.bindingS (AST.BS-let ce x₁) ∷L stmnts₁) e)) x =
  --      let x₁' = evalPureExpr x₁ (proj₁ (proj₁ x))
  --          e' = SubstOne.substOneExpr (inl x₁')
  --                 ((AST.body (AST.bodyR (stmnts₁) e))) 
  --      in dec-rec ⟨ IsPureE e' ⟩ {{proj₁ (snd (IsPureE e'))}}
  --         (λ x₂ → evalPureExpr e' x₂)
  --         subst-preserver-pure 

  --     where
  --       postulate subst-preserver-pure : _
        
  --   evalPureExpr (AST.body (AST.bodyR (AST.bindingS (AST.BS-publish! p x₁) ∷L stmnts₁) expr₁)) ((() , x₃) , x₂)
       
  --   evalPureExpr (AST.body (AST.bodyR (AST.nonBindingS _ ∷L stmnts₁) expr₁)) x =
  --      evalPureExpr (AST.body (AST.bodyR (stmnts₁) expr₁)) ((proj₂ (proj₁ x)) , (proj₂ x))

    
  --   evalPureExpr (AST.lit x₁) x = x₁
  --   evalPureExpr (AST.if e then e₁ else e₂) x =
  --      Cubical.Data.Bool.if evalPureExpr e (proj₁ x)
  --         then evalPureExpr e₁ (proj₁ (proj₂ x))
  --         else evalPureExpr e₂ (proj₂ (proj₂ x))
  --   evalPureExpr (AST._$'_ f xs) x =
  --      let z = BuiltIn'.impl (snd (BuiltIns'.getBi builtIns (AST.BI.bIndex f)))
  --          q = appV z (evalArgs xs) 
  --      in subst-GTypeAgdaRep (sym (AST.BI.cdm≡ f)) q
  --        --(transport⁻ (cong GTypeAgdaRep (AST.BI.cdm≡ f)) q)
  --   evalPureExpr (AST.var (AST.dsot name {inl ()})) tt
  --   evalPureExpr {sc = sc} (AST.sign q {z} {p}) w =
  --       subst-GTypeAgdaRep p (signPrim (AST.pId-name _ _ _ (IsNotConsensus→Participant
  --          {con [] sc}
  --            z)) (evalPureArg q))


{-# OPTIONS --cubical  #-}
module Glow.Simple.ParamsSubst where

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

open import Glow.Simple.AST

open import Glow.DecEqMore








module ParamsSubst {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}} where


  open AST Identifier

  -- open InteractionHead

  stripParamsHead : InteractionHead → InteractionHead 
  stripParamsHead ih = interactionHead (participants ih) []


  stripParamsCtx : ∀ {ih : _} → Context ih → Context (stripParamsHead ih)
  stripParamsCtx Γ = con (Γ .entries) (Γ .scope')

  -- TODO : remove unsafe pragma by stratification on nesting depth
  {-# TERMINATING #-}
  paramSubst : ∀ {ih : _} → ParametersValue (parameters ih) → 
                   ∀ {Γ : _} →  Statements _ Γ → Statements _ (stripParamsCtx Γ) 



  paramSubst {ih}  vv = map-Linked'-map _ h hh
    where



      h : {Γ : Context ih}
             → (b : Stmnt ih Γ) → Stmnt _ (stripParamsCtx Γ)

      h-expr : {Γ : Context ih} → ∀ {Τ}
             → (b : Expr ih Γ Τ) → Expr _ (stripParamsCtx Γ) Τ


      h  (bindingS x) = bindingS (BS-lemma x)
         where
              BS-lemma : {Γ : Context ih} →  BStmnt ih Γ -> BStmnt _ (stripParamsCtx Γ)
              BS-lemma (BS-let x {asn} y) = (BS-let x {asn} (h-expr y))  
              BS-lemma (BS-publish! p (psof name₁ {w}) {y}) = (BS-publish! p (psof name₁ {w}) {y})


      h (nonBindingS x) = nonBindingS (z x)
         where

           zz : NBStmnt _ _ → NBStmnt _ _ 
           zz (NBS-require! x) = NBS-require! (h-expr x)
           zz (NBS-deposit! p {y} x) = NBS-deposit! p {y} (h-expr x)
           zz (NBS-withdraw! p {y} x) = NBS-withdraw! p {y} (h-expr x)

           z : NBStmnt+Expr ih _ → NBStmnt+Expr (stripParamsHead ih) _
           z (stmntNBS x) =  stmntNBS (zz x)
           z (exprNBS x) = exprNBS (h-expr x)

      h-expr (var (dsot x {y})) =
         sum-elim
           (λ a → var (dsot x {fromWitness (inl a)}))
           (lit ∘ (lookup-ParametersValue (ih .parameters) vv (iwt x _)) ∘ proj₂)
            (toWitness y)
        

      h-expr (stmnts₁ ;b x) =
         paramSubst vv stmnts₁ ;b subst (λ x₁ → Expr _ x₁ _)
             -- TODO : improve evaluation performance by introducing specialized "subst"
             -- specialisation should be not pnly on Expr, but also on map-Linked'-map-fold
        (map-Linked'-map-fold (stripParamsCtx {ih}) _ _ stmnts₁ ) (h-expr x)
      h-expr (lit x) = lit x

      hh : (Γ : Context ih) (x : Stmnt _ Γ) →
         stripParamsCtx (bindingMechanics' _ Γ x) ≡
         bindingMechanics' (interactionHead (participants ih) [])
         (stripParamsCtx Γ) (h x)
      hh _ (bindingS (BS-let _ _)) = refl 
      hh _ (bindingS (BS-publish! _ _)) =  refl 
      hh _ (nonBindingS _) = refl


module Test-String where
  open AST String {{String-Discrete-postulated}}

  module ParamsSubstS = ParamsSubst {{String-Discrete-postulated}}

  someInteraction : Interaction
  someInteraction =  
     interaction⟨   "A" ∷ "B" ∷ [] ,  "pI1" ∶ Nat ∷ "b2" ∶ Bool ∷ "b1" ∶ Bool ∷ [] ⟩ (
          set "x" ∶ Bool ≔ < true > ;
          at "B" set "y" ∶ Bool ≔ v "b1" ;
          at "A" set "xx" ∶ Bool ≔ (
              require! v "b2" ;'
              -- publish! "B" ⟶ "y" ;
              -- withdraw! "B" ⟵ < 3 > ;
              -- deposit! "B" ⟶ < 2 > ;
              set "z" ∶ Bool ≔ < false > ;b
              < true >
              );
          deposit! "B" ⟶ < 2 > ;
          withdraw! "B" ⟵ < 3 > ;
          publish! "B" ⟶ "y" ;'        
          set "yy" ∶ Bool ≔ v "y" )


  param-sub-test : ℕ × 𝟚 × 𝟚 × Unit → Linked'
                                        (bindingMechanics'
                                         (ParamsSubstS.stripParamsHead
                                          (interactionHead ("A" ∷ "B" ∷ [])
                                           ("pI1" ∶ Nat ∷ "b2" ∶ Bool ∷ "b1" ∶ Bool ∷ []))))
                                        (ParamsSubstS.stripParamsCtx (Interaction.emptyContext someInteraction))
  param-sub-test vv = ParamsSubstS.paramSubst vv (Interaction.code someInteraction)


  zzz :
    let q : ℕ × 𝟚 × 𝟚 × Unit
        q = 3 , false , true , _
        bT : Statements _ _
        bT = (
          set "x" ∶ Bool ≔ < true > ;
          at "B" set "y" ∶ Bool ≔ < true > ;
          at "A" set "xx" ∶ Bool ≔ (
              require! < false > ;'
              -- publish! "B" ⟶ "y" ;
              -- withdraw! "B" ⟵ < 3 > ;
              -- deposit! "B" ⟶ < 2 > ;
              set "z" ∶ Bool ≔ < false > ;b
              < true >
              );
          deposit! "B" ⟶ < 2 > ;
          withdraw! "B" ⟵ < 3 > ;
          publish! "B" ⟶ "y" ;'        
          set "yy" ∶ Bool ≔ v "y"
          )
    in bT ≡ param-sub-test q 

  zzz = refl



-- module Test-ℕ where
--   open AST ℕ 

--   module ParamsSubstS = ParamsSubst {ℕ}

--   someInteraction : Interaction
--   someInteraction =  
--      interaction⟨   1 ∷ 2 ∷ [] ,  3 ∶ Nat ∷ 4 ∶ Bool ∷ 5 ∶ Bool ∷ [] ⟩ (
--           set 6 ∶ Bool ≔ < true > ;
--           at 2 set 7 ∶ Bool ≔ v 5 ;
--           at 1 set 8 ∶ Bool ≔ (
--               require! v 4 ;'
--               -- publish! "B" ⟶ "y" ;
--               -- withdraw! "B" ⟵ < 3 > ;
--               -- deposit! "B" ⟶ < 2 > ;
--               set 9 ∶ Bool ≔ < false > ;b
--               < true >
--               );
--           deposit! 2 ⟶ < 2 > ;
--           withdraw! 2 ⟵ < 3 > ;
--           publish! 2 ⟶ 7 ;'        
--           set 10 ∶ Bool ≔ v 7 )


--   param-sub-test : ℕ × 𝟚 × 𝟚 × Unit → Linked'
--                                         (bindingMechanics'
--                                          (ParamsSubstS.stripParamsHead
--                                           (interactionHead (1 ∷ 2 ∷ [])
--                                            (3 ∶ Nat ∷ 4 ∶ Bool ∷ 5 ∶ Bool ∷ []))))
--                                         (ParamsSubstS.stripParamsCtx (Interaction.emptyContext someInteraction))
--   param-sub-test vv = ParamsSubstS.paramSubst vv (Interaction.code someInteraction)

--   zzz :
--     let q : ℕ × 𝟚 × 𝟚 × Unit
--         q = 3 , false , true , _
--     in (
--           set 6 ∶ Bool ≔ < true > ;
--           at 2 set 7 ∶ Bool ≔ < true > ;
--           at 1 set 8 ∶ Bool ≔ (
--               require! < false > ;'
--               -- publish! "B" ⟶ "y" ;
--               -- withdraw! "B" ⟵ < 3 > ;
--               -- deposit! "B" ⟶ < 2 > ;
--               set 9 ∶ Bool ≔ < false > ;b
--               < true >
--               );
--           deposit! 2 ⟶ < 2 > ;
--           withdraw! 2 ⟵ < 3 > ;
--           publish! 2 ⟶ 7 ;'        
--           set 10 ∶ Bool ≔ v 7 ) ≡ param-sub-test q 

--   zzz = refl


--   -- zzz2 : Type₀
--   -- zzz2 =
--   --   ∀ x y z → param-sub-test (x , y , z , _) ≡ (
--   --       set "x" ∶ Bool ≔ < true > ;
--   --       at "B" set "y" ∶ Bool ≔ < z > ;
--   --       at "A" set "xx" ∶ Bool ≔ (
--   --           require! < y > ;'
--   --           -- publish! "B" ⟶ "y" ;
--   --           -- withdraw! "B" ⟵ < 3 > ;
--   --           -- deposit! "B" ⟶ < 2 > ;
--   --           set "z" ∶ Bool ≔ < false > ;b
--   --           < true >
--   --           );
--   --       deposit! "B" ⟶ < 2 > ;
--   --       withdraw! "B" ⟵ < 3 > ;
--   --       publish! "B" ⟶ "y" ;'        
--   --       set "yy" ∶ Bool ≔ v "y" )

--   -- zzz2' : zzz2
--   -- zzz2' _ _ _ = refl

{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.ExpressionsExamples where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Fin
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool hiding (if_then_else_)  renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Cubical.HITs.Interval

open import Glow.Simple.AST

import Glow.Simple.ASTDef

open import Glow.Simple.Lurk.Translation

open import Glow.Simple.Lurk.HaskellInterface

open import Glow.Simple.Lurk.Translation

module examples where
  open Glow.Simple.ASTDef.AST-String zero

  ih = (interactionHead [] [])
  Γ : Context ih
  Γ = con [] nothing

  e0 : Expr ih Γ Bool 
  e0 =  < false >

  e1 : Expr ih (con [ AST.ice nothing "q" Bool ] nothing) Bool 
  e1 =  "and" $ (va "q" , va "q")

  e2 : Expr ih (con (AST.ice nothing "b2" Bool ∷ AST.ice nothing "y" Bool ∷ []) nothing) Bool 
  e2 =  ((set "xx" ∶ Bool ≔
         ( if (bi "and") $' ((var-a (dsot "y")) , var-a (dsot "b2"))

           then
              (
              set "z" ∶ Bool ≔ < false > ;₁ ;b
              v "z"
            )
           else (
            set "q" ∶ Bool ≔ < false > ;'

            set "z" ∶ Bool ≔  "and" $ (va "q" , va "q") ;b
            < true >
            )) 
         ) ;₁) ;b (v "xx") 
  


module transl {A : Type₀} (a : A) where
  open Glow.Simple.ASTDef.AST-String one


  transl : ∀ {Τ Γ} → Expr (interactionHead [] []) Γ Τ → LurkASTnoRecords.Expr String A
  transl x = Translate.translateE {A = A} a "unimplemented" bi-renderer x
     where
        bi-renderer : (Τ : GType) → BI Τ → String
        bi-renderer Τ (AST.bi' bIndex) = bIndex



  



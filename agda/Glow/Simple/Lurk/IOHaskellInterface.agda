{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.IOHaskellInterface where

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

open import Glow.Simple.Lurk.HaskellInterface

-- IOTCM "IOHaskellInterface.agda" None Indirect (Cmd_compute_toplevel DefaultCompute "test1")

-- echo "IOTCM \"IOHaskellInterface.agda\" None Indirect (Cmd_compute_toplevel DefaultCompute \"test1\")" | agda --interaction

open import Cubical.HITs.Interval

open import Cubical.Relation.Nullary renaming (¬_ to IsEmpty)

open import Glow.Simple.Postulates

open import Glow.DecEqMore

instance
  String-Discrete-postulated-unsafe : IsDiscrete String
  String-Discrete-postulated-unsafe = String-Discrete-postulated


import Glow.Simple.AST

data TranslationUnit : Type₀ where
  TU : TranslationUnit

import Glow.Simple.Lurk.ExpressionsExamples

module examples = Glow.Simple.Lurk.ExpressionsExamples.examples

translate : ∀ {Τ : Glow.Simple.AST.GType} → ∀ {Γ : Glow.Simple.AST.AST.Context examples.ih} → Glow.Simple.AST.AST.Expr examples.ih Γ Τ → LurkASTnoRecords.Expr String TranslationUnit
translate  {Τ} {Γ} x = Glow.Simple.Lurk.ExpressionsExamples.transl.transl TU (snd (snd (Glow.Simple.AST.toProofsE  _ _ {Τ = Τ} (_ , (Γ , x)))))

open LurkASTnoRecords String TranslationUnit public





test1 : Expr
test1 = ExBinary TU BOpCons (ExSymbol TU (SymbolC "d")) (ExSymbol TU (SymbolC "b"))

test2 : Expr
test2 = ExLambda TU (toList ((SymbolC "d1") ∷ (SymbolC "d2") ∷ [])) test1

example0 = translate examples.e0
example1 = translate examples.e1
example2 = translate examples.e2

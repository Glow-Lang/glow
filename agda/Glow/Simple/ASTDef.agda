
{-# OPTIONS --cubical  #-}
module Glow.Simple.ASTDef where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod renaming (map to map-prod)
open import Cubical.Data.Sum renaming (elim to sum-elim ; map to map-sum)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool hiding (if_then_else_) renaming (Bool to 𝟚 ; _≟_ to _≟B_)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )

open import Cubical.HITs.Interval

-- open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary renaming (¬_ to IsEmpty)
open import Cubical.Relation.Binary

open import Glow.Linked

open import Glow.DecEqMore

open import Glow.Simple.AST



instance
  String-Discrete-postulated-unsafe : IsDiscrete String
  String-Discrete-postulated-unsafe = String-Discrete-postulated


Basic-BuiltIns : BuiltIns' String
BuiltIns'.getBi Basic-BuiltIns = h

  where
    h : _
    h "not" = ([ Bool ] , Bool) , (builitIn not)
    h "and" = ( Bool ∷  [ Bool ] , Bool) , (builitIn _and_)
    h "or" = ( Bool ∷  [ Bool ] , Bool) , (builitIn _or_)
    h "+ℕ" = ( Nat ∷  [ Nat ] , Nat) , (builitIn Cubical.Data.Nat._+_)
    h "randomUInt256" = ([] , Int) , (builitIn randomUInt256Prim)
    h _ = ([] , Unitᵍ) , builitIn _




module AST-String = AST String Basic-BuiltIns 


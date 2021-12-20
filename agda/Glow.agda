-- cubical demo

{-# OPTIONS --cubical --no-import-sorts #-}
module Cubical.Experiments.Glow where

open import Agda.Builtin.String
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List
open import Cubical.Data.Maybe


-- postulate String : Type₀
 

data S-expr : Type₀ where
  〔〕 : S-expr


data 𝑮Participant : Type₀ where

data 𝑮Identifier : Type₀ where


data 𝑮Expr : Type₀ where

data 𝑮Val : Type₀ where

data 𝑮Label : Type₀ where


𝑮Statements : Type₀

data 𝑮Statement : Type₀ where
   -let : 𝑮Identifier → 𝑮Expr → 𝑮Statement
   -case : List (𝑮Expr × 𝑮Statements) → 𝑮Statement
   -withdraw : Maybe 𝑮Label → 𝑮Participant → 𝑮Expr → 𝑮Statement 
   -deposit : Maybe 𝑮Label → 𝑮Participant → 𝑮Expr → 𝑮Statement 
   -publish : Maybe 𝑮Label → 𝑮Participant → 𝑮Identifier → 𝑮Statement

𝑮Statements = List 𝑮Statement


record 𝑮Interaction : Type₀ where
  constructor 𝒈Interaction
  field
    stmnts : List 𝑮Participant
    parameters : List (𝑮Identifier)    
    participants : List (𝑮Identifier)    


record EContext : Type₀ where
  constructor eContext
  field
    definedVars : List (𝑮Identifier × 𝑮Val)    
    

record State : Type₀ where
  constructor state
  field
    parametersVals : List 𝑮Val
    stmnts : 𝑮Statements
    ctx : EContext

data RuntimeError : Type₀ where

evalExpr : State → 𝑮Expr → 𝑮Val 
evalExpr = {!!}


execution : State → RuntimeError ⊎ State 
execution = {!!}

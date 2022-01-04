
{-# OPTIONS --cubical  #-}
module Glow.ASTRaw where

open import Agda.Builtin.String
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List
open import Cubical.Data.Maybe
open import Cubical.Data.Bool

open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

-- based on glow/compiler/parse/expressions.ss

-- this is provided for the compatiblity with Glow parser, programs in this representaiton
-- will undergo series of translations into more and more type safe representations,
-- where each step may fail or produce safer version of program.


data Token : Type₀ where
  statement-with-attribute : Token
  attribute : Token
  identifier : Token
  list-expression : Token
  function-definition : Token
  hash-f : Token
  body-expression : Token
  expression-statement : Token
  deposit-expression : Token
  numeric-literal : Token
  value-definition : Token
  call-expression : Token
  arguments : Token
  string-literal : Token
  publish-statement : Token
  if-expression : Token
  equality-expression : Token
  withdraw-expression : Token

data Raw : Type₀ where
    𝓔 : List Raw → Raw 
    𝓛 : List Raw → Raw
    𝑺 : String → Raw
    𝑰 : ℤ → Raw
    𝑻 : Token → Raw 


raw-test : Raw
raw-test =
 (𝓛 
   ( (𝓔 
   ( (𝑻 statement-with-attribute) ∷ (𝓔 
   ( (𝑻 attribute) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "interaction") ∷ [] ) ) ∷ (𝓛 
   ( (𝓔 
   ( (𝑻 list-expression) ∷ (𝓛 
   ( (𝓔  ( (𝑻 identifier) ∷ (𝑺 "A") ∷ [] ) ) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "B") ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 function-definition) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "interaction") ∷ [] ) ) ∷ (𝓛 []) ∷ (𝑻 hash-f) ∷ (𝓔 
   ( (𝑻 body-expression) ∷ (𝓛 
   ( (𝓔 
   ( (𝑻 expression-statement) ∷ (𝓔 
   ( (𝑻 deposit-expression) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "A") ∷ [] ) ) ∷ (𝓔  ( (𝑻 numeric-literal) ∷ (𝑰 1) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 expression-statement) ∷ (𝓔 
   ( (𝑻 deposit-expression) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "B") ∷ [] ) ) ∷ (𝓔  ( (𝑻 numeric-literal) ∷ (𝑰 1) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 statement-with-attribute) ∷ (𝓔 
   ( (𝑻 attribute) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "A") ∷ [] ) ) ∷ (𝑻 hash-f) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 value-definition) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "a") ∷ [] ) ) ∷ (𝑻 hash-f) ∷ (𝓔 
   ( (𝑻 call-expression) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "input") ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 arguments) ∷ (𝓛 
   ( (𝓔  ( (𝑻 identifier) ∷ (𝑺 "Bool") ∷ [] ) ) ∷ (𝓔  ( (𝑻 string-literal) ∷ (𝑺 "Enter A's choice.") ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 publish-statement) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "A") ∷ [] ) ) ∷ (𝓛  ( (𝓔  ( (𝑻 identifier) ∷ (𝑺 "a") ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 statement-with-attribute) ∷ (𝓔 
   ( (𝑻 attribute) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "B") ∷ [] ) ) ∷ (𝑻 hash-f) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 value-definition) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "b") ∷ [] ) ) ∷ (𝑻 hash-f) ∷ (𝓔 
   ( (𝑻 call-expression) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "input") ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 arguments) ∷ (𝓛 
   ( (𝓔  ( (𝑻 identifier) ∷ (𝑺 "Bool") ∷ [] ) ) ∷ (𝓔  ( (𝑻 string-literal) ∷ (𝑺 "Enter B's choice.") ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 publish-statement) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "B") ∷ [] ) ) ∷ (𝓛  ( (𝓔  ( (𝑻 identifier) ∷ (𝑺 "b") ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 if-expression) ∷ (𝓔 
   ( (𝑻 equality-expression) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "a") ∷ [] ) ) ∷ (𝑺 "==") ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "b") ∷ [] ) ) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 body-expression) ∷ (𝓛 
   ( (𝓔 
   ( (𝑻 expression-statement) ∷ (𝓔 
   ( (𝑻 withdraw-expression) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "A") ∷ [] ) ) ∷ (𝓔  ( (𝑻 numeric-literal) ∷ (𝑰 2) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ (𝑻 hash-f) ∷ [] ) ) ∷ (𝓔 
   ( (𝑻 body-expression) ∷ (𝓛 
   ( (𝓔 
   ( (𝑻 expression-statement) ∷ (𝓔 
   ( (𝑻 withdraw-expression) ∷ (𝓔  ( (𝑻 identifier) ∷ (𝑺 "B") ∷ [] ) ) ∷ (𝓔  ( (𝑻 numeric-literal) ∷ (𝑰 2) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ (𝑻 hash-f) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) ) ∷ [] ) )

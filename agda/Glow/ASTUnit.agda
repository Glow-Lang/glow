
{-# OPTIONS --cubical  #-}
module Glow.AST where

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


module GlowAST (IdentifierTy : Type₀) where

  data Expression : Type₀

  data Identifier : Type₀ where
    identifier : IdentifierTy → Identifier

  data Op : Type₀ where
    op : IdentifierTy → Op

  data Literal : Type₀ where
    boolean-literal : Bool → Literal
    numeric-literal : ℤ → Literal
    string-literal : String → Literal


  data Attribute : Type₀ where
     attribute : Identifier → Maybe (List Expression) → Attribute


  data GType : Type₀ where
    type-name : IdentifierTy → GType
    type-var : IdentifierTy → GType
    type-tuple : List IdentifierTy → GType
    type-record : List (IdentifierTy × GType) → GType
    type-with-attribute : Attribute → GType → GType
    
  data Arguments : Type₀ where
     arguments : List Expression → Arguments


  data Pattern : Type₀ where
    𝓟-type-annotation-pattern : Pattern → GType → Pattern
    𝓟-pattern-id : Identifier → Pattern
    𝓟-pattern-blank : Pattern
    𝓟-pattern-lit : Literal  → Pattern
    𝓟-pattern-tuple : List Pattern → Pattern
    𝓟-pattern-or : List Pattern → Pattern
    𝓟-pattern-list : List Pattern → Pattern
    𝓟-pattern-record : (List (Identifier × Pattern))  → Pattern
    𝓟-pattern-app-ctor : Identifier → (List Pattern) → Pattern
    𝓟-pattern-with-attribute : Attribute → Pattern → Pattern


  data Case : Type₀ where
    case : Pattern → Expression → Case


  data Variant : Type₀ where
    V-Identifier : Identifier → Variant
    V-Identifier' : Identifier → List GType → Variant


  data Param : Type₀ where
    param : Identifier → (Maybe GType) → Param


  data Statement : Type₀ where
    𝓢-publish-statement : Identifier → (List Identifier) → Statement 
    𝓢-verify-statement : List Identifier → Statement
    𝓢-type-alias-declaration : Identifier → (Maybe (List Identifier)) → GType → Statement
    𝓢-data-type-declaration : Identifier → (Maybe (List Variant)) → Statement
    𝓢-expression-statement : Expression → Statement
    𝓢-value-definition : Identifier → (Maybe GType) →  Expression → Statement
    𝓢-function-definition : Identifier → (List Param) → (Maybe GType) →  Expression → Statement
    𝓢-statement-with-attribute : Attribute → Statement → Statement

  data Expression where
     𝓔-Identifier : Identifier → Expression
     𝓔-Literal : Literal → Expression
     𝓔-binary-expression : Expression → Op → Expression → Expression
     𝓔-unary-expression : Op → Expression → Expression
     𝓔-tuple-expression : List Expression → Expression
     𝓔-list-expression : List Expression → Expression
     𝓔-record-expression : List (Identifier × Expression) → Expression
     𝓔-call-expression : Expression → Arguments → Expression
     𝓔-require-expression : Expression → Expression
     𝓔-assert-expression : Expression → Expression
     𝓔-deposit-expression : Identifier → Expression → Expression
     𝓔-withdraw-expression : Identifier → Expression → Expression
     𝓔-dot-expression : Expression → Identifier → Expression
     𝓔-type-annotation-expression : Expression → GType → Expression
     𝓔-body-expression : (List Statement) → Maybe Expression → Expression
     𝓔-if-expression : Expression → Expression → Expression → Expression
     𝓔-switch-expression : Expression →  (List Case) → Expression
     𝓔-expression-with-attribute : Attribute → Expression → Expression


  App = List Statement

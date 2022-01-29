
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

open import Glow.Linked

-- based on glow/compiler/parse/expressions.ss

-- this is provided for the compatiblity with Glow parser, programs in this representaiton
-- will undergo series of translations into more and more type safe representations,
-- where each step may fail or produce safer version of program.


myTuple : ℕ × 𝔹
myTuple = ?




-- module GlowAST (IdentifierTy : Type₀)
--                (Context : Type₀)
--                (empty-context : Context)
--                (ConTrans : Context → Type₀)
--                (conTrans : (Γ : Context) → (ct : ConTrans Γ) → Context) 
--                (Typed : Type₀)
--                  where

--   record Typing : Type₀


--   data Expression (Γ : Context) (Τ : Typed) : Type₀
 
--   data Identifier (Γ : Context) (Τ : Typed) : Type₀ where
--     identifier : IdentifierTy → Identifier Γ Τ

--   data Op (Γ : Context) (Τ : Typed) : Type₀ where
--     op : IdentifierTy → Op Γ Τ

--   data Literal (Γ : Context) (Τ : Typed) : Type₀ where
--     boolean-literal : Bool → Literal Γ Τ
--     numeric-literal : ℤ → Literal Γ Τ
--     string-literal : String → Literal Γ Τ


--   data Attribute (Γ : Context) (Τ : Typed) : Type₀ where
--      attribute : Identifier Γ Τ → Maybe (List (Σ _ (Expression Γ))) → Attribute Γ Τ


--   data GType (Γ : Context) (Τ : Typed) : Type₀ where
--     type-name : IdentifierTy → GType Γ Τ
--     type-var : IdentifierTy → GType Γ Τ
--     type-tuple : List (IdentifierTy) → GType Γ Τ
--     type-record : List (IdentifierTy × GType Γ Τ) → GType Γ Τ
--     type-with-attribute : Attribute Γ Τ → GType Γ Τ → GType Γ Τ
    
--   data Arguments (Γ : Context) (Τ : Typed) : Type₀ where
--      arguments : List (Σ _ (Expression Γ)) → Arguments Γ Τ


--   data Pattern (Γ : Context) (Τ : Typed) : Type₀ where
--     𝓟-type-annotation-pattern : Pattern Γ Τ → GType Γ Τ → Pattern Γ Τ
--     𝓟-pattern-id : Identifier Γ Τ → Pattern Γ Τ
--     𝓟-pattern-blank : Pattern Γ Τ
--     𝓟-pattern-lit : Literal Γ Τ  → Pattern Γ Τ
--     𝓟-pattern-tuple : List (Pattern Γ Τ) → Pattern Γ Τ
--     𝓟-pattern-or : List (Pattern Γ Τ) → Pattern Γ Τ
--     𝓟-pattern-list : List (Pattern Γ Τ) → Pattern Γ Τ
--     𝓟-pattern-record : (List (Identifier Γ Τ × Pattern Γ Τ))  → Pattern Γ Τ
--     𝓟-pattern-app-ctor : Identifier Γ Τ → (List (Pattern Γ Τ)) → Pattern Γ Τ
--     𝓟-pattern-with-attribute : Attribute Γ Τ → Pattern Γ Τ → Pattern Γ Τ


--   data Case (Γ : Context) (Τ : Typed) : Type₀ where
--     case : Pattern Γ Τ → Expression Γ Τ → Case Γ Τ


--   data Variant (Γ : Context) (Τ : Typed) : Type₀ where
--     V-Identifier : Identifier Γ Τ → Variant Γ Τ
--     V-Identifier' : Identifier Γ Τ → List (GType Γ Τ) → Variant Γ Τ


--   data Param (Γ : Context) (Τ : Typed) : Type₀ where
--     param : Identifier Γ Τ → (Maybe (GType Γ Τ)) → Param Γ Τ


--   data Statement (Γ : Context) (Τ : Typed) : Type₀ where
--     𝓢-publish-statement : (Identifier Γ Τ) → (List (Identifier Γ Τ)) → Statement Γ Τ 
--     𝓢-verify-statement : List (Identifier Γ Τ) → Statement Γ Τ
--     𝓢-type-alias-declaration : (Identifier Γ Τ) → (Maybe (List (Identifier Γ Τ)))
--                                     → (GType  Γ Τ) → Statement Γ Τ
--     𝓢-data-type-declaration : Identifier Γ Τ → (Maybe (List (Variant Γ Τ))) → Statement Γ Τ
--     𝓢-expression-statement : Expression Γ Τ → Statement Γ Τ
--     𝓢-value-definition : Identifier Γ Τ → (Maybe (GType Γ Τ)) →  Expression Γ Τ → Statement Γ Τ
--     𝓢-function-definition : Identifier Γ Τ → (List (Param Γ Τ))
--           → (Maybe (GType Γ Τ)) →  (Expression Γ Τ) → Statement Γ Τ
--     𝓢-statement-with-attribute : Attribute Γ Τ → Statement Γ Τ → Statement Γ Τ

--   data Expression Γ Τ where
--      𝓔-Identifier : Identifier Γ Τ → Expression Γ Τ
--      𝓔-Literal : Literal Γ Τ → Expression Γ Τ
--      𝓔-binary-expression : (Expression Γ Τ) → (Op Γ Τ) → (Expression Γ Τ) → Expression Γ Τ
--      𝓔-unary-expression : (Op Γ Τ) → (Expression Γ Τ) → Expression Γ Τ
--      𝓔-tuple-expression : List (Expression Γ Τ) → Expression Γ Τ
--      𝓔-list-expression : List (Expression Γ Τ) → Expression Γ Τ
--      𝓔-record-expression : List (Identifier Γ Τ × Expression Γ Τ) → Expression Γ Τ
--      𝓔-call-expression : Expression Γ Τ → Arguments Γ Τ → Expression Γ Τ
--      𝓔-require-expression : Expression Γ Τ → Expression Γ Τ
--      𝓔-assert-expression : Expression Γ Τ → Expression Γ Τ
--      𝓔-deposit-expression : Identifier Γ Τ → Expression Γ Τ → Expression Γ Τ
--      𝓔-withdraw-expression : Identifier Γ Τ → Expression Γ Τ → Expression Γ Τ
--      𝓔-dot-expression : Expression Γ Τ → Identifier Γ Τ → Expression Γ Τ
--      𝓔-type-annotation-expression : Expression Γ Τ → GType Γ Τ → Expression Γ Τ
--      𝓔-body-expression : (List (Statement Γ Τ)) → Maybe (Expression Γ Τ) → Expression Γ Τ
--      𝓔-if-expression : Expression Γ Τ → Expression Γ Τ → Expression Γ Τ → Expression Γ Τ
--      𝓔-switch-expression : Expression Γ Τ →  (List (Case Γ Τ)) → Expression Γ Τ
--      𝓔-expression-with-attribute : Attribute Γ Τ → Expression Γ Τ → Expression Γ Τ

--   record Typing where 
--     field
--       Γ : {!!}
--       Τ : {!!}
--       T-identifier : IdentifierTy → Identifier Γ Τ

--     -- data Op (Γ : Context) (Τ : Typed) : Type₀ where
--       T-op : IdentifierTy → Op Γ Τ

--     -- data Literal (Γ : Context) (Τ : Typed) : Type₀ where
--       T-boolean-literal : Bool → Literal Γ Τ
--       T-numeric-literal : ℤ → Literal Γ Τ
--       T-string-literal : String → Literal Γ Τ


--     -- data Attribute (Γ : Context) (Τ : Typed) : Type₀ where
--       T-attribute : Identifier Γ Τ → Maybe (List (Σ _ (Expression Γ))) → Attribute Γ Τ


--     -- data GType (Γ : Context) (Τ : Typed) : Type₀ where
--       T-type-name : IdentifierTy → GType Γ Τ
--       T-type-var : IdentifierTy → GType Γ Τ
--       T-type-tuple : List (IdentifierTy) → GType Γ Τ
--       T-type-record : List (IdentifierTy × GType Γ Τ) → GType Γ Τ
--       T-type-with-attribute : Attribute Γ Τ → GType Γ Τ → GType Γ Τ

--     -- data Arguments (Γ : Context) (Τ : Typed) : Type₀ where
--       T-arguments : List (Σ _ (Expression Γ)) → Arguments Γ Τ


--     -- data Pattern (Γ : Context) (Τ : Typed) : Type₀ where
--       T-𝓟-type-annotation-pattern : Pattern Γ Τ → GType Γ Τ → Pattern Γ Τ
--       T-𝓟-pattern-id : Identifier Γ Τ → Pattern Γ Τ
--       T-𝓟-pattern-blank : Pattern Γ Τ
--       T-𝓟-pattern-lit : Literal Γ Τ  → Pattern Γ Τ
--       T-𝓟-pattern-tuple : List (Pattern Γ Τ) → Pattern Γ Τ
--       T-𝓟-pattern-or : List (Pattern Γ Τ) → Pattern Γ Τ
--       T-𝓟-pattern-list : List (Pattern Γ Τ) → Pattern Γ Τ
--       T-𝓟-pattern-record : (List (Identifier Γ Τ × Pattern Γ Τ))  → Pattern Γ Τ
--       T-𝓟-pattern-app-ctor : Identifier Γ Τ → (List (Pattern Γ Τ)) → Pattern Γ Τ
--       T-𝓟-pattern-with-attribute : Attribute Γ Τ → Pattern Γ Τ → Pattern Γ Τ


--     -- data Case (Γ : Context) (Τ : Typed) : Type₀ where
--       T-case : Pattern Γ Τ → Expression Γ Τ → Case Γ Τ


--     -- data Variant (Γ : Context) (Τ : Typed) : Type₀ where
--       T-V-Identifier : Identifier Γ Τ → Variant Γ Τ
--       T-V-Identifier' : Identifier Γ Τ → List (GType Γ Τ) → Variant Γ Τ


--     -- data Param (Γ : Context) (Τ : Typed) : Type₀ where
--       T-param : Identifier Γ Τ → (Maybe (GType Γ Τ)) → Param Γ Τ


--     -- data Statement (Γ : Context) (Τ : Typed) : Type₀ where
--       T-𝓢-publish-statement : (Identifier Γ Τ) → (List (Identifier Γ Τ)) → Statement Γ Τ 
--       T-𝓢-verify-statement : List (Identifier Γ Τ) → Statement Γ Τ
--       T-𝓢-type-alias-declaration : (Identifier Γ Τ) → (Maybe (List (Identifier Γ Τ)))
--                                       → (GType  Γ Τ) → Statement Γ Τ
--       T-𝓢-data-type-declaration : Identifier Γ Τ → (Maybe (List (Variant Γ Τ))) → Statement Γ Τ
--       T-𝓢-expression-statement : Expression Γ Τ → Statement Γ Τ
--       T-𝓢-value-definition : Identifier Γ Τ → (Maybe (GType Γ Τ)) →  Expression Γ Τ → Statement Γ Τ
--       T-𝓢-function-definition : Identifier Γ Τ → (List (Param Γ Τ))
--             → (Maybe (GType Γ Τ)) →  (Expression Γ Τ) → Statement Γ Τ
--       T-𝓢-statement-with-attribute : Attribute Γ Τ → Statement Γ Τ → Statement Γ Τ

--     -- data Expression Γ Τ where

--       T-𝓔-Identifier : Identifier Γ Τ → Expression Γ Τ
--       T-𝓔-Literal : Literal Γ Τ → Expression Γ Τ
--       T-𝓔-binary-expression : (Expression Γ Τ) → (Op Γ Τ) → (Expression Γ Τ) → Expression Γ Τ
--       T-𝓔-unary-expression : (Op Γ Τ) → (Expression Γ Τ) → Expression Γ Τ
--       T-𝓔-tuple-expression : List (Expression Γ Τ) → Expression Γ Τ
--       T-𝓔-list-expression : List (Expression Γ Τ) → Expression Γ Τ
--       T-𝓔-record-expression : List (Identifier Γ Τ × Expression Γ Τ) → Expression Γ Τ
--       T-𝓔-call-expression : Expression Γ Τ → Arguments Γ Τ → Expression Γ Τ
--       T-𝓔-require-expression : Expression Γ Τ → Expression Γ Τ
--       T-𝓔-assert-expression : Expression Γ Τ → Expression Γ Τ
--       T-𝓔-deposit-expression : Identifier Γ Τ → Expression Γ Τ → Expression Γ Τ
--       T-𝓔-withdraw-expression : Identifier Γ Τ → Expression Γ Τ → Expression Γ Τ
--       T-𝓔-dot-expression : Expression Γ Τ → Identifier Γ Τ → Expression Γ Τ
--       T-𝓔-type-annotation-expression : Expression Γ Τ → GType Γ Τ → Expression Γ Τ
--       T-𝓔-body-expression : (List (Statement Γ Τ)) → Maybe (Expression Γ Τ) → Expression Γ Τ
--       T-𝓔-if-expression : Expression Γ Τ → Expression Γ Τ → Expression Γ Τ → Expression Γ Τ
--       T-𝓔-switch-expression : Expression Γ Τ →  (List (Case Γ Τ)) → Expression Γ Τ
--       T-𝓔-expression-with-attribute : Attribute Γ Τ → Expression Γ Τ → Expression Γ Τ



--   App : Context → Type
--   App Γ = List (Σ _ (Statement Γ))

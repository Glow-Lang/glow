
{-# OPTIONS --cubical  #-}
module Glow.AST2 where

open import Agda.Builtin.String
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List renaming (map to map-List)
open import Cubical.Data.Maybe
open import Cubical.Data.Bool

open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

-- based on glow/compiler/parse/expressions.ss

-- this is provided for the compatiblity with Glow parser, programs in this representaiton
-- will undergo series of translations into more and more type safe representations,
-- where each step may fail or produce safer version of program.


module GlowAST (IdentifierTy : Type₀)
               (D : Type₀)
                 where

  record T : Type₁
  
  record T where 
    field
  --  data Identifier (d : D) : Type₀ where
      T-identifier : IdentifierTy → D → Type₀

  --  data Op (d : D) : Type₀ where
      T-op : IdentifierTy  → D → Type₀

  --  data Literal (d : D) : Type₀ where
      T-boolean-literal : Bool  → D → Type₀
      T-numeric-literal : ℤ  → D → Type₀
      T-string-literal : String → D → Type₀


  --  data Attribute (d : D) : Type₀ where
      T-attribute : D → Maybe (List D) → D → Type₀


  --  data GType (d : D) : Type₀ where
      T-type-name : IdentifierTy → D → Type₀
      T-type-var : IdentifierTy  → D → Type₀
      T-type-tuple : List (IdentifierTy)  → D → Type₀
      T-type-record : List (IdentifierTy × D)  → D → Type₀
      T-type-with-attribute : D → D  → D → Type₀

  --  data Arguments (d : D) : Type₀ where
      T-arguments : List D  → D → Type₀


  --  data Pattern (d : D) : Type₀ where
      T-𝓟-type-annotation-pattern : D → D  → D → Type₀
      T-𝓟-pattern-id : D  → D → Type₀
      T-𝓟-pattern-blank : D → Type₀
      T-𝓟-pattern-lit : D   → D → Type₀
      T-𝓟-pattern-tuple : List D  → D → Type₀
      T-𝓟-pattern-or : List D  → D → Type₀
      T-𝓟-pattern-list : List D  → D → Type₀
      T-𝓟-pattern-record : (List (D × D))  → D → Type₀
      T-𝓟-pattern-app-ctor : D → (List D)  → D → Type₀
      T-𝓟-pattern-with-attribute : D → D  → D → Type₀


  --  data Case (d : D) : Type₀ where
      T-case : D → D  → D → Type₀


  --  data Variant (d : D) : Type₀ where
      T-V-Identifier : D  → D → Type₀
      T-V-Identifier' : D → List D  → D → Type₀


  --  data Param (d : D) : Type₀ where
      T-param : D → (Maybe D)  → D → Type₀


  --  data Statement (d : D) : Type₀ where
      T-𝓢-publish-statement : D → (List D)  → D → Type₀ 
      T-𝓢-verify-statement : List D  → D → Type₀
      T-𝓢-type-alias-declaration : D → (Maybe (List D))
                                      → D  → D → Type₀
      T-𝓢-data-type-declaration : D → (Maybe (List D))  → D → Type₀
      T-𝓢-expression-statement : D  → D → Type₀
      T-𝓢-value-definition : D → (Maybe  D) →  D  → D → Type₀
      T-𝓢-function-definition : D → (List D)
            → (Maybe D) →  D  → D → Type₀
      T-𝓢-statement-with-attribute : D → D  → D → Type₀

  --  data Expression d where
      T-𝓔-Identifier : D  → D → Type₀
      T-𝓔-Literal : D → D → Type₀
      T-𝓔-binary-expression : D → D → D  → D → Type₀
      T-𝓔-unary-expression : D → D  → D → Type₀
      T-𝓔-tuple-expression : List D  → D → Type₀
      T-𝓔-list-expression : List D  → D → Type₀
      T-𝓔-record-expression : List (D × D)  → D → Type₀
      T-𝓔-call-expression : D → D  → D → Type₀
      T-𝓔-require-expression : D  → D → Type₀
      T-𝓔-assert-expression : D  → D → Type₀
      T-𝓔-deposit-expression : D → D  → D → Type₀
      T-𝓔-withdraw-expression : D → D  → D → Type₀
      T-𝓔-dot-expression : D → D  → D → Type₀
      T-𝓔-type-annotation-expression : D → D  → D → Type₀
      T-𝓔-body-expression : (List D) → Maybe D  → D → Type₀
      T-𝓔-if-expression : D → D → D  → D → Type₀
      T-𝓔-switch-expression : D → (List D)  → D → Type₀
      T-𝓔-expression-with-attribute : D → D  → D → Type₀

    data Expression (d : D) : Type₀

    data Identifier (d : D) : Type₀

    data Op (d : D) : Type₀ 

    data Literal (d : D) : Type₀

    data Attribute (d : D) : Type₀ 

    data GType (d : D) : Type₀ 

    data Arguments (d : D) : Type₀ 

    data Pattern (d : D) : Type₀

    data Case (d : D) : Type₀ 

    data Variant (d : D) : Type₀ 

    data Param (d : D) : Type₀ 

    data Statement (d : D) : Type₀

    data Identifier d where
      identifier : (x : IdentifierTy) → T-identifier x d → Identifier d

    data Op d where
      op : (x : IdentifierTy) → T-op x d  → Op d

    data Literal d where
      boolean-literal : (x : Bool) → (T-boolean-literal x d) → Literal d
      numeric-literal : (x : ℤ) → (T-numeric-literal x d) → Literal d
      string-literal : (x : String) → (T-string-literal x d) → Literal d


    data Attribute d where
      attribute : (x : Σ _ Identifier) → (y : Maybe (List (Σ _ (Expression))))
                    → T-attribute (fst x) (map-Maybe (map-List fst) y) d
                    → Attribute d


    data GType d where
      type-name : (x : IdentifierTy) → GType d
      type-var : (x : IdentifierTy) → GType d
      type-tuple : (x : List (IdentifierTy)) → GType d
      type-record : (x : List (IdentifierTy × Σ _ GType)) → GType d
      type-with-attribute : (x : Σ _ Attribute) → (y : Σ _ GType) → GType d

    data Arguments d where
       arguments : List (Σ _ Expression) → Arguments d


    data Pattern d where
      𝓟-type-annotation-pattern : Pattern d → GType d → Pattern d
      𝓟-pattern-id : Identifier d → Pattern d
      𝓟-pattern-blank : Pattern d
      𝓟-pattern-lit : Literal d  → Pattern d
      𝓟-pattern-tuple : List (Pattern d) → Pattern d
      𝓟-pattern-or : List (Pattern d) → Pattern d
      𝓟-pattern-list : List (Pattern d) → Pattern d
      𝓟-pattern-record : (List (Identifier d × Pattern d))  → Pattern d
      𝓟-pattern-app-ctor : Identifier d → (List (Pattern d)) → Pattern d
      𝓟-pattern-with-attribute : Attribute d → Pattern d → Pattern d


    data Case d where
      case : Pattern d → Expression d → Case d


    data Variant d where
      V-Identifier : Identifier d → Variant d
      V-Identifier' : Identifier d → List (GType d) → Variant d


    data Param d where
      param : Identifier d → (Maybe (GType d)) → Param d


    data Statement d where
      𝓢-publish-statement : (Identifier d) → (List (Identifier d)) → Statement d 
      𝓢-verify-statement : List (Identifier d) → Statement d
      𝓢-type-alias-declaration : (Identifier d) → (Maybe (List (Identifier d)))
                                      → (GType  d) → Statement d
      𝓢-data-type-declaration : Identifier d → (Maybe (List (Variant d))) → Statement d
      𝓢-expression-statement : Expression d → Statement d
      𝓢-value-definition : Identifier d → (Maybe (GType d)) →  Expression d → Statement d
      𝓢-function-definition : Identifier d → (List (Param d))
            → (Maybe (GType d)) →  (Expression d) → Statement d
      𝓢-statement-with-attribute : Attribute d → Statement d → Statement d

    data Expression d where
       𝓔-Identifier : Identifier d → Expression d
       𝓔-Literal : Literal d → Expression d
       𝓔-binary-expression : (Expression d) → (Op d) → (Expression d) → Expression d
       𝓔-unary-expression : (Op d) → (Expression d) → Expression d
       𝓔-tuple-expression : List (Expression d) → Expression d
       𝓔-list-expression : List (Expression d) → Expression d
       𝓔-record-expression : List (Identifier d × Expression d) → Expression d
       𝓔-call-expression : Expression d → Arguments d → Expression d
       𝓔-require-expression : Expression d → Expression d
       𝓔-assert-expression : Expression d → Expression d
       𝓔-deposit-expression : Identifier d → Expression d → Expression d
       𝓔-withdraw-expression : Identifier d → Expression d → Expression d
       𝓔-dot-expression : Expression d → Identifier d → Expression d
       𝓔-type-annotation-expression : Expression d → GType d → Expression d
       𝓔-body-expression : (List (Statement d)) → Maybe (Expression d) → Expression d
       𝓔-if-expression : Expression d → Expression d → Expression d → Expression d
       𝓔-switch-expression : Expression d →  (List (Case d)) → Expression d
       𝓔-expression-with-attribute : Attribute d → Expression d → Expression d

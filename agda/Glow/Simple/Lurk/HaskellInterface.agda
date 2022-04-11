{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.HaskellInterface where

open import Agda.Builtin.String
open import Agda.Builtin.Char
import Agda.Builtin.List as L

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Fin
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)


open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool hiding (if_then_else_)  renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

data BinOp : Type₀
data UnaryOp : Type₀

data BinOp where
  BOpCons BOpPlus BOpMinus BOpTimes BOpDiv BOpNumEq  BOpPtrEq : BinOp


data UnaryOp where
  UOpCar UOpCdr UOpEmit : UnaryOp


module LurkAST (List' : Type₀ → Type₀) (SymbolIdentifier : Type₀)  (a : Type₀) where

  open import Cubical.Data.List renaming (map to map-List)


  data Expr : Type₀

  record Let : Type₀
  record Binding : Type₀
  record Symbol : Type₀

  data Expr where
     ExNil : a → Expr
     ExT : a → Expr
     ExIf : a → Expr → Expr → Expr → Expr
     ExLambda : a → List' Symbol → Expr → Expr
     ExLet : a → Let → Expr
     ExLetRec : a → Let → Expr
     ExBinary : a → BinOp → Expr → Expr → Expr
     ExUnary : a → UnaryOp → Expr → Expr
     ExBegin : a → List' Expr → Expr → Expr
     ExCurrentEnv : a → Expr
     ExFieldElem : a → ℕ → Expr
     ExEval : a → Expr → (Maybe Expr) → Expr
     ExSymbol : a → Symbol → Expr
     ExApply : a → Expr →  List' Expr → Expr
     ExQuotedName : a → String → Expr
     -- ExQuote : a → String → Expr 

  {-# NO_POSITIVITY_CHECK #-}
  record Let where
    constructor LetC
    inductive
    field
      letBindings : List' Binding
      letBody : Expr

  record Binding where
    constructor BindingC
    inductive
    field
      bInfo : a
      bKey : Symbol
      bVal : Expr

  record Symbol where
    constructor SymbolC
    inductive
    field
      name : SymbolIdentifier





module LurkASTchangeListImp {SymbolIdentifier : Type₀} {a : Type₀}  (List₀ List₁ : Type₀ → Type₀)
                               (lmap : ∀ {X Y} → (X → Y) → List₀ X → List₀ Y)
                               (to₁ : ∀ {X} → List₀ X → List₁ X) where

  module L₀ = LurkAST List₀ SymbolIdentifier a
  module L₁ = LurkAST List₁ SymbolIdentifier a

  {-# TERMINATING #-}
  mapLiImp : L₀.Expr → L₁.Expr 
  mapLiImp (LurkAST.ExNil x) = (LurkAST.ExNil x)
  mapLiImp (LurkAST.ExT x) = (LurkAST.ExT x)
  mapLiImp (LurkAST.ExIf x x₁ x₂ x₃) = (LurkAST.ExIf x (mapLiImp x₁) (mapLiImp x₂) (mapLiImp x₃))
  mapLiImp (LurkAST.ExLambda x x₁ x₂) =
     (LurkAST.ExLambda x (to₁ (lmap (λ x₃ → LurkAST.SymbolC (L₀.Symbol.name x₃)) x₁)) (mapLiImp x₂))
  mapLiImp (LurkAST.ExLet x (w)) = 
      (LurkAST.ExLet x (LurkAST.LetC (to₁ (lmap (λ x₁ → LurkAST.BindingC (bInfo x₁) (LurkAST.SymbolC (L₀.Symbol.name (bKey x₁))) (mapLiImp (bVal x₁))) letBindings))
          (mapLiImp letBody)))
      where
        open L₀.Let w
        open L₀.Binding
        
  mapLiImp (LurkAST.ExLetRec x w) =
       (LurkAST.ExLetRec x (LurkAST.LetC (to₁ (lmap (λ x₁ → LurkAST.BindingC (bInfo x₁) (LurkAST.SymbolC (L₀.Symbol.name (bKey x₁))) (mapLiImp (bVal x₁))) letBindings))
          (mapLiImp letBody)))
      where
        open L₀.Let w
        open L₀.Binding

  mapLiImp (LurkAST.ExBinary x x₁ x₂ x₃) = (LurkAST.ExBinary x x₁ (mapLiImp x₂) (mapLiImp x₃))
  mapLiImp (LurkAST.ExUnary x x₁ x₂) = (LurkAST.ExUnary x x₁ (mapLiImp x₂))
  mapLiImp (LurkAST.ExBegin x x₁ x₂) = (LurkAST.ExBegin x (to₁ (lmap mapLiImp x₁)) (mapLiImp x₂))
  mapLiImp (LurkAST.ExCurrentEnv x) = (LurkAST.ExCurrentEnv x)
  mapLiImp (LurkAST.ExFieldElem x x₁) = (LurkAST.ExFieldElem x x₁)
  mapLiImp (LurkAST.ExEval x x₁ nothing) = (LurkAST.ExEval x (mapLiImp x₁) nothing)
  mapLiImp (LurkAST.ExEval x x₁ (just x₂)) = (LurkAST.ExEval x (mapLiImp x₁) (just (mapLiImp x₂)))
  mapLiImp (LurkAST.ExSymbol x (LurkAST.SymbolC name)) = (LurkAST.ExSymbol x (LurkAST.SymbolC name))
  mapLiImp (LurkAST.ExApply x x₁ x₂) = (LurkAST.ExApply x (mapLiImp x₁) (to₁ (lmap mapLiImp x₂)))
  mapLiImp (LurkAST.ExQuotedName x x₁) = (LurkAST.ExQuotedName x x₁)
  -- mapLiImp (LurkAST.ExQuote x x₁) = (LurkAST.ExQuote x x₁)


data AList {ℓ} (b : Type ℓ) : Type ℓ where
   ALn : AList b
   ALc : b → (AList b) → AList b



fromAList : ∀ {ℓ} {b : Type ℓ} → AList b → L.List b 
fromAList ALn = L.[]
fromAList {b = b} (ALc x x₁) = x L.∷ (fromAList {b = b} x₁) 

toAList : ∀ {ℓ} {b : Type ℓ} → L.List b → AList b 
toAList L.[] = ALn
toAList {b = b} (x L.∷ x₁) = ALc x (toAList {b = b} x₁)


-- module LurkASTnoRecords {ℓ} (SymbolIdentifier : Type₀) (a : Type ℓ) where



--   import Cubical.Data.List as L renaming (map to map-List)

--   data Expr : Type ℓ
--   data BinOp : Type ℓ
--   data UnaryOp : Type ℓ
--   data Let : Type ℓ
--   data Binding : Type ℓ
--   data Symbol : Type₀ 

--   data Expr where
--      ExNil : a → Expr
--      ExT : a → Expr
--      ExIf : a → Expr → Expr → Expr → Expr
--      ExLambda : a → List Symbol → Expr → Expr
--      ExLet : a → Let → Expr
--      ExLetRec : a → Let → Expr
--      ExBinary : a → BinOp → Expr → Expr → Expr
--      ExUnary : a → UnaryOp → Expr → Expr
--      ExBegin : a → List Expr → Expr → Expr
--      ExCurrentEnv : a → Expr

--      ExEval : a → Expr → (Maybe Expr) → Expr
--      ExSymbol : a → Symbol → Expr
--      ExApply : a → Expr →  List Expr → Expr
--      ExQuotedName : a → String → Expr
--      -- ExQuote : a SExpr : 

--   data BinOp where
--     BOpCons BOpPlus BOpMinus BOpTimes BOpDiv BOpNumEq  BOpPtrEq : BinOp


--   data UnaryOp where
--     UOpCar UOpCdr UOpEmit : UnaryOp

--   data Let where
--     LetC : List Binding → Expr → Let


--   data Binding where
--     BindingC : a → Symbol → Expr → Binding

--   data Symbol where
--     SymbolC : SymbolIdentifier → Symbol 




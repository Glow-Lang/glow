-- cubical demo

{-# OPTIONS --cubical --no-import-sorts #-}
module Glow.SExpr2 where

open import Agda.Builtin.String
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List
open import Cubical.Data.Maybe


infixr 5 _،_
infixr 5 _ₛ،_


data SExprs (Token : Type₀) : Type₀
data SExpr (Token : Type₀) : Type₀

data SExprs Token where
  _،_ : SExpr Token → SExprs Token → SExprs Token
  _ₛ،_ : Token → SExprs Token → SExprs Token
  _〕 : SExpr Token → SExprs Token
  _ₛ〕 : Token → SExprs Token
  
data SExpr Token  where
  〔_ : SExprs Token → SExpr Token
  〔〕 : SExpr Token
  
infixl 3 〔_

infix 9 _〕 
infix 9 _ₛ〕 


-- infir 8 ₛ_


test-SExprs : SExpr ℕ
test-SExprs = 〔 2 ₛ،  〔〕 ،  3 ₛ، 3 ₛ〕



test-SExprs2 : SExpr ℕ
test-SExprs2 = 〔 test-SExprs ، (〔  test-SExprs ،  test-SExprs ،  test-SExprs ، test-SExprs  〕) ،  test-SExprs ، test-SExprs  〕


-- -- test-SExprs' : SExpr ℕ
-- -- test-SExprs' = 〔 0 ،  1 ،  2 ، 3  〕


-- -- test-SExprs : SExpr ℕ
-- -- test-SExprs = 〔 (1 ، (2 ، (3 ، (4 〕))) )


-- -- record GlowModel : Type₁ where 
-- --   field
-- --     Program : Type₀
-- --     ParametersValue : Program → Type₀
-- --     State : (p : Program) → ParametersValue p → Type₀
-- --     RuntimeError : Type₀
-- --     initialize : (p : Program) → (pv : ParametersValue p) → State p pv
-- --     ExecutionPath : (p : Program) → ParametersValue p → Type₀
-- --     ProgramPredicate : Program → Type₀
-- --     ProgramPredicatePred : (p : Program) → ProgramPredicate p → Type₀

    

-- --     -- initialize : Program → State 
-- --     -- ExecutionPath : Type₀    

-- -- -- postulate  : Type₀

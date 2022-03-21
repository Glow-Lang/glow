
{-# OPTIONS --cubical  #-}
module Glow.Simple.TraceNiceTest where

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
open import Glow.Simple.ASTDef

open import Glow.Simple.Example

open import Glow.Simple.TraceNice

module TestTraceNice where 
  -- open AST String {{String-Discrete-postulated}} one


  open TraceNice {String} {String} {Basic-BuiltIns} {ptps =  ("A" , honest) ∷ ("B" , honest) ∷ []}


  

  module test1 where

  traceTestTy : 𝟚 × Unit → EState → Type₀
  traceTestTy = snd (genTracesType ((toProofs _ _ someInteraction))) 
     -- (Trace nothing {!(someCode)!})

  -- -- tyEvalTest : Type₀
  -- -- tyEvalTest = traceTestTy (2 , (false , (true , tt)))

  tyEvalTest : 𝟚 × Unit → EState → Type₀
    -- traceTestTy es
  tyEvalTest (p1 , p2) es = ΣM (Input "B" Bool)
                              (λ x →
                                 ×M (Publish "B" "y")
                                 (ΣM
                                  (λ es →
                                     Branch (Input "A" Bool es) Unit (ok-input-elim "B" Bool x and p1))
                                  (λ x₁ →
                                     ×M (Deposit "B" 2)
                                     (ΣM (Input "A" Bool)
                                      (λ x₂ →
                                         ×M (Withdraw "A" 3) (×M (Deposit "A" 3) (Publish "A" "xx")))))))
                              es

  tyEvalTestOk : 𝟚 × Unit → Type₀
  tyEvalTestOk (p1 , p2) = ΣM (Input "B" Bool)
                             (λ x →
                                ×M (Publish "B" "y")
                                (ΣM
                                 (λ es →
                                    Branch (Input "A" Bool es) Unit (ok-input-elim "B" Bool x and p1))
                                 (λ x₁ →
                                    ×M (Deposit "B" 2)
                                    (ΣM (Input "A" Bool)
                                     (λ x₂ →
                                        ×M (Withdraw "A" 3) (×M (Deposit "A" 3) (Publish "A" "xx")))))))
                             ok

  tyEvalTestFail : 𝟚 × Unit → Type₀
  tyEvalTestFail = {!!}

  --                 -- ΣM (Input "B" Bool)
  --                 --   (λ x →
  --                 --      ×M (Publish "B" "y")
  --                 --      (ΣM
  --                 --       (λ es →
  --                 --          Branch (Input "A" Bool es) (Require true es)
  --                 --          (ok-input-elim "B" Bool x))
  --                 --       (λ x₁ →
  --                 --          ×M (Deposit "B" 2)
  --                 --          (ΣM (Input "A" Bool)
  --                 --           (λ x₂ →
  --                 --              ×M (Withdraw "A" 3) (×M (Deposit "A" 3) (Publish "A" "xx")))))))
  --                 --   es


  -- -- someTrace :  Σ _ (tyEvalTest)
  -- -- someTrace = 
  -- --     ok ,   "B" inp true
  -- --           ↦ p! "B" ⤇ "y"
  -- --           ↦ br-T refl 
  -- --                 ("A" inp false)
  -- --           ↦ d! "B" ⤇ 2
  -- --           ↦ "A" inp false
  -- --           ↦ w! "A" ⤆ 3
  -- --           ↦ d! "A" ⤇ 3
  -- --           ↦ p! "A" ⤇ "xx"

  -- -- traceTestCases : tyEvalTest ok → Unit 
  -- -- traceTestCases ((."B" inp x₁) ↦ (p! ."B" ⤇ ."y") ↦ br-T prf-T (."A" inp x) ↦ (d! ."B" ⤇ .2) ↦ (."A" inp x₃) ↦ (w! ."A" ⤆ .3) ↦ (d! ."A" ⤇ .3) ↦ (p! ."A" ⤇ ."xx")) = {!!}
  -- -- traceTestCases ((."B" inp x₁) ↦ (p! ."B" ⤇ ."y") ↦ br-F prf-F (r! .true) ↦ (d! ."B" ⤇ .2) ↦ (."A" inp x₂) ↦ (w! ."A" ⤆ .3) ↦ (d! ."A" ⤇ .3) ↦ (p! ."A" ⤇ ."xx")) = {!!}


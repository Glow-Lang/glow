
{-# OPTIONS --cubical  #-}
module Glow.Simple.TraceExample where

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

open import Glow.Simple.Example

open import Glow.Simple.Trace

module TestTrace where 
  open AST String {{String-Discrete-postulated}} one


  open Trace {{String-Discrete-postulated}} {ptps =  "A" ∷ "B" ∷ []}

  traceTestTy : ℕ × 𝟚 × 𝟚 × Unit → Type₀
  traceTestTy p = Trace _ (someCode p)

  -- tyEvalTest : Type₀
  -- tyEvalTest = traceTestTy (2 , (false , (true , tt)))

  tyEvalTest : ℕ → 𝟚 → 𝟚 →  Type₀
  tyEvalTest x x₁ x₂ = Σ {ℓ-zero} {ℓ-zero} (Maybe {ℓ-zero} 𝟚)
                         (λ x →
                            recMaybe {ℓ-suc ℓ-zero} {Type} {ℓ-zero} {𝟚} Unit
                            (λ v →
                               Σ {ℓ-zero} {ℓ-zero} 𝟚
                               (λ x₁ →
                                  Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero} {Type} x₁
                                  (Σ {ℓ-zero} {ℓ-zero}
                                   (fst
                                    (Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero}
                                     {Σ {ℓ-suc ℓ-zero} {ℓ-zero} Type (λ Tr → Tr → Maybe {ℓ-zero} 𝟚)} v
                                     (Σ {ℓ-zero} {ℓ-zero} (Maybe {ℓ-zero} 𝟚)
                                      (λ x₂ →
                                         recMaybe {ℓ-suc ℓ-zero} {Type} {ℓ-zero} {𝟚} Unit (λ v₁ → Unit) x₂)
                                      ,
                                      (λ x₂ →
                                         maybe-elim {ℓ-zero} {𝟚}
                                         {λ b' →
                                            recMaybe {ℓ-suc ℓ-zero} {Type} {ℓ-zero} {𝟚} Unit (λ v₁ → Unit) b' →
                                            Maybe {ℓ-zero} 𝟚}
                                         (λ x₃ → nothing) (λ v₁ _ → just v₁) (fst x₂) (snd x₂)))
                                     (Σ {ℓ-zero} {ℓ-zero} 𝟚
                                      (λ x₂ →
                                         Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero} {Type} x₂ Unit Unit)
                                      ,
                                      (λ x₂ →
                                         bindMaybe {Unit} {𝟚}
                                         (𝟚-elim {ℓ-zero}
                                          {λ x₃ →
                                             Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero} {Type} x₃ Unit
                                             Unit →
                                             Maybe {ℓ-zero} Unit}
                                          (λ _ → nothing) just (fst x₂) (snd x₂))
                                         (λ _ → just true)))))
                                   (λ x₂ →
                                      recMaybe {ℓ-suc ℓ-zero} {Type} {ℓ-zero} {𝟚} Unit
                                      (λ v₁ →
                                         Σ {ℓ-zero} {ℓ-zero} 𝟚
                                         (λ x₃ →
                                            Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero} {Type} x₃
                                            (Σ {ℓ-zero} {ℓ-zero} (Maybe {ℓ-zero} 𝟚)
                                             (λ x₄ →
                                                recMaybe {ℓ-suc ℓ-zero} {Type} {ℓ-zero} {𝟚} Unit
                                                (λ v₂ →
                                                   Σ {ℓ-zero} {ℓ-zero} 𝟚
                                                   (λ x₅ →
                                                      Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero} {Type} x₅
                                                      (Σ {ℓ-zero} {ℓ-zero} 𝟚
                                                       (λ x₆ →
                                                          Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero} {Type} x₆
                                                          (Σ {ℓ-zero} {ℓ-zero} 𝟚
                                                           (λ x₇ →
                                                              Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero} {Type} x₇ Unit
                                                              Unit))
                                                          Unit))
                                                      Unit))
                                                x₄))
                                            Unit))
                                      (snd
                                       (Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero}
                                        {Σ {ℓ-suc ℓ-zero} {ℓ-zero} Type (λ Tr → Tr → Maybe {ℓ-zero} 𝟚)} v
                                        (Σ {ℓ-zero} {ℓ-zero} (Maybe {ℓ-zero} 𝟚)
                                         (λ x₃ →
                                            recMaybe {ℓ-suc ℓ-zero} {Type} {ℓ-zero} {𝟚} Unit (λ v₁ → Unit) x₃)
                                         ,
                                         (λ x₃ →
                                            maybe-elim {ℓ-zero} {𝟚}
                                            {λ b' →
                                               recMaybe {ℓ-suc ℓ-zero} {Type} {ℓ-zero} {𝟚} Unit (λ v₁ → Unit) b' →
                                               Maybe {ℓ-zero} 𝟚}
                                            (λ x₄ → nothing) (λ v₁ _ → just v₁) (fst x₃) (snd x₃)))
                                        (Σ {ℓ-zero} {ℓ-zero} 𝟚
                                         (λ x₃ →
                                            Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero} {Type} x₃ Unit Unit)
                                         ,
                                         (λ x₃ →
                                            bindMaybe {Unit} {𝟚}
                                            (𝟚-elim {ℓ-zero}
                                             {λ x₄ →
                                                Cubical.Data.Bool.if_then_else_ {ℓ-suc ℓ-zero} {Type} x₄ Unit
                                                Unit →
                                                Maybe {ℓ-zero} Unit}
                                             (λ _ → nothing) just (fst x₃) (snd x₃))
                                            (λ _ → just true))))
                                       x₂)))
                                  Unit))
                            x)


  -- -- traceTestCases : traceTestTy (2 , (false , (true , tt))) → 𝟚 
  -- -- traceTestCases (false , snd₁) = {!snd₁!}
  -- -- traceTestCases (true , snd₁) = {!!}

  traceTestCases : ∀ x y z → tyEvalTest x y z → 𝟚
  traceTestCases x y z (nothing , tt) = {!!}
  traceTestCases x y z (just x₁ , false , tt) = {!!}
  traceTestCases x y z (just false , true , (false , snd₂) , snd₁) = {!!}
  traceTestCases x y z (just false , true , (true , snd₂) , snd₁) = {!!}
  traceTestCases x y z (just true , true , (nothing , snd₂) , snd₁) = {!!}
  traceTestCases x y z (just true , true , (just x₁ , snd₂) , snd₁) = {!!}
  
  -- traceTestCases : tyEvalTest → 𝟚 
  -- traceTestCases (nothing , snd₁) = {!!}
  -- traceTestCases (just x , false , tt) = {!!}
  -- traceTestCases (just false , true , (false , snd₂) , snd₁) = {!!}
  -- traceTestCases (just false , true , (true , snd₂) , snd₁) = {!!}
  -- traceTestCases (just true , true , (nothing , tt) , tt) = {!!}
  -- traceTestCases (just true , true , (just x , tt) , fst₁ , snd₁) = {!!}



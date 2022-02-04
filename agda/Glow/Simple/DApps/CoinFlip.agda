
{-# OPTIONS --cubical  #-}
module Glow.Simple.DApps.CoinFlip where

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

open import Cubical.Data.Vec

open import Glow.Linked

open import Glow.DecEqMore

open import Glow.Simple.AST

open import Glow.Simple.Postulates

open import Glow.Simple.ASTDef
  
open import Glow.Simple.TraceNice


open AST-String zero

someInteraction : Interaction
someInteraction =  
   interaction⟨   "A" ∷ "B" ∷ [] ,  ("wagerAmount" ∶ Nat) ∷ ("escrowAmount" ∶ Nat) ∷ [] ⟩ (
        
        at "A" set "randA" ∶ Nat ≔ ("randomUInt256" $ tt) ;
        at "A" set "commitment" ∶ Digest ≔ ("digestNat" $ (va "randA")) ;
        publish! "A" ⟶ "commitment" ;

        deposit! "A" ⟶ ("+ℕ" $ (va "wagerAmount" , va "escrowAmount")) ;

        at "B" set "randB" ∶ Nat ≔ ("randomUInt256" $ tt) ;
        publish! "B" ⟶ "randB" ;        
        deposit! "B" ⟶ v "wagerAmount" ;

        publish! "A" ⟶ "randA" ;

        set "r" ∶ Digest ≔ ("digestNat" $ (va "randA")) ;
        require! ("==Digest" $ (va "r" , va "commitment")) ;

        set "n0" ∶ Nat ≔ ("^^^" $ (va "randA" , va "randB")) ;
        set "n1" ∶ Nat ≔ ("&&&" $ (va "n0" , lit-a 1)) ;'
        nonBindingS (exprNBS (( if "==Nat" $ ((va "n1") , lit-a 0) 
           then
              (
               set "w1" ∶ Nat ≔ ("*ℕ" $ (lit-a 2 , va "wagerAmount")) ;
               set "w2" ∶ Nat ≔ ("+ℕ" $ (va "w1" , va "escrowAmount")) ;'             
               withdraw! "A" ⟵ v "w2" ;b
               < tt >
            )
           else (
               set "w1" ∶ Nat ≔ ("*ℕ" $ (lit-a 2 , va "wagerAmount")) ;
               withdraw! "B" ⟵ v "w1" ;'
               withdraw! "A" ⟵ v "escrowAmount" ;b
               < tt >
            ))))
        )


ePaths : ℕ × ℕ × Unit → EState → Type
ePaths = snd (genTracesType ((toProofs _ _ someInteraction)))

ePaths' : EState → ℕ × ℕ →  Type
-- ePaths' = {!!} -- ePaths (p1 , p2 , tt) es
ePaths' es (p1 , p2) = ×M (Publish "A" "commitment")
                         (×M (Deposit "A" (p1 Cubical.Data.Nat.+ p2))
                          (×M (Publish "B" "randB")
                           (×M (Deposit "B" p1)
                            (×M (Publish "A" "randA")
                             (×M
                              (Require
                               (digestEqTestPrim (digestPrim randomUInt256Prim)
                                (digestPrim randomUInt256Prim)))
                              (λ es →
                                 Branch
                                 (Withdraw "A"
                                  (p1 Cubical.Data.Nat.+ (p1 Cubical.Data.Nat.+ 0) Cubical.Data.Nat.+
                                   p2)
                                  es)
                                 (×M
                                  (Withdraw "B" (p1 Cubical.Data.Nat.+ (p1 Cubical.Data.Nat.+ 0)))
                                  (Withdraw "A" p2) es)
                                 (Dec→Bool
                                  (discreteℕ
                                   (&&&Prim (^^^Prim randomUInt256Prim randomUInt256Prim) 1) 0))))))))
                         es

-- -- -- ×M (Publish "A" "commitment")
-- -- --                (×M (Deposit "A" 0)
-- -- --                 (×M (Publish "B" "randB")
-- -- --                  (×M (Deposit "B" 0)
-- -- --                   (×M (Publish "A" "randA")
-- -- --                    (×M
-- -- --                     (Require
-- -- --                      (digestEqTestPrim (digestPrim randomUInt256Prim)
-- -- --                       (transp (λ i → Dig) i0
-- -- --                        (transp (λ i → Dig) i0
-- -- --                         (transp (λ i → Dig) i0
-- -- --                          (transp (λ i → Dig) i0
-- -- --                           (transp (λ i → Dig) i0
-- -- --                            (transp (λ i → Dig) i0
-- -- --                             (transp (λ i → Dig) i0
-- -- --                              (transp (λ i → Dig) i0
-- -- --                               (transp (λ i → Dig) i0
-- -- --                                (transp (λ i → Dig) i0
-- -- --                                 (transp (λ i → Dig) i0 (digestPrim randomUInt256Prim))))))))))))))
-- -- --                     (λ es →
-- -- --                        Branch (Withdraw "A" 0 es)
-- -- --                        (×M (Withdraw "B" 0) (Withdraw "A" 0) es)
-- -- --                        (Dec→Bool
-- -- --                         (discreteℕ
-- -- --                          (&&&Prim (^^^Prim randomUInt256Prim randomUInt256Prim) 1) 0))))))))
-- -- --                ok



test1 : ∀ es → (Σ _ (ePaths' es)) → 𝟚
test1 fail ((x , x₁) , (x₂ □)) = {!!}
test1 fail ((x , x₁) , a ↦ snd₁) = {!!}
test1 ok ((x , x₁) , a ↦ a₁ ↦ a₂ ↦ a₃ ↦ a₄ ↦ a₅ ↦ br-T prf-T x₂) = {!!}
test1 ok ((x , x₁) , a ↦ a₁ ↦ a₂ ↦ a₃ ↦ a₄ ↦ a₅ ↦ br-F prf-F (a₆ ↦ x₂)) = {!!}



zzzz : Vec Unit 4 → 𝟚
zzzz x = {!x!}
-- -- -- -- test1 : ePaths (zero , zero , tt) ok → 𝟚
-- -- -- -- test1 x = {!x!}

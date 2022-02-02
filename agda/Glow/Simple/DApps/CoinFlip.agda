
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

open import Glow.Linked

open import Glow.DecEqMore

open import Glow.Simple.AST

-- open import Glow.Simple.ParamsSubst

open import Glow.Simple.ASTDef
  

open AST-String zero

someInteraction : Interaction
someInteraction =  
   interaction⟨   "A" ∷ "B" ∷ [] ,  ("wagerAmount" , Nat) ∷ ("escrowAmount" , Nat) ∷ [] ⟩ (
        
        at "A" set "randA" ∶ Int ≔ (randomUInt256 $' tt) ;
        at "A" set "commitment" ∶ Int ≔ (digest $' (var-a "randA")) ;
        publish! "A" ⟶ "commitment" ;        
        deposit! "A" ⟶ (+ℕ $' (var-a "wagerAmount" , var-a "escrowAmount")) 
        at "B" set "randB" ∶ Int ≔ (randomUInt256 $' tt) ;
        publish! "B" ⟶ "randB" ;        
        deposit! "B" ⟶ "wagerAmount" ;

        publish! "A" ⟶ "randA" ;

        set "r" ∶ Digest ≔ (digest $' (var-a "randA")) ;
        require! (==Digest $' (var-a "r" , var-a "commitment")) ;

        set "b0" ≔ (^^^ $' (var-a "randA" , var-a "randB")) ;
        ( if (&&& $' (var-a "b0" , lit-a < 1 >)) 
           then
              (
               set "w1" ∶ Digest ≔ (*ℕ $' (lit-a < 2 > , var-a "wagerAmount")) ;
               set "w2" ∶ Digest ≔ (+ℕ $' (var-a "w1" , var-a "escrowAmount")) ;b             
               withdraw! "A" ⟵ "w2" 
            )
           else (
               set "w1" ∶ Digest ≔ (*ℕ $' (lit-a < 2 > , var-a "wagerAmount")) ;
               withdraw! "B" ⟵ "w1" ;b
               withdraw! "A" ⟵ "escrowAmount"
            )) ;
        )

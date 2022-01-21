
{-# OPTIONS --cubical  #-}
module Glow.Simple.Monad where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool hiding (if_then_else_)  renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.Simple.AST

open import Glow.DecEqMore

open import Glow.Simple.ContextMore

open import Cubical.HITs.Interval


module Deep where
  -- deep embeding, forgetfull, not necessary usefull for proofs,
  -- I am not shure if it will be usefull but it is easy to define
  data G (A : Type₀) : Type₁ where
    input : ∀ {A'} → String → {{IsGlowTy A'}} → A ≡ Maybe A'  → G A
    withdraw : 𝟚 ≡ A → G A
    deposit : 𝟚 ≡ A → G A
    _>>=_ : ∀ {A'} → G A' → (A' → G A) → G A
    -- end : A ≡ Unit → G A

  input' : String → (Τ : GType) → G (Maybe (GTypeAgdaRep Τ)) 
  input' x Τ = input x {{GTypeAgdaRep' Τ}} refl

  -- _>>=_ : ∀ {A B : Type₀} → G A → (A → G B) → G B
  -- _>>=_ = {!!}

  _>>_ : ∀ {A B : Type₀} → G A → G B → G B
  x >> x₁ = x >>= const x₁


  -- doTest : G {!!}
  -- doTest = do
  --    z ← input' "xxx" Bool
  --    g z
  negTest' : ∀ {A} → G A → Type₀


  exec : ∀ {A} → (x : G A) → (negTest' x) → A

  negTest' (input {A'} x x₁) = Maybe A'
  negTest' (withdraw x) = 𝟚
  negTest' (deposit x) = 𝟚
  negTest' (x >>= x₁) = Σ (negTest' x) λ x₂ → negTest' (x₁ (exec x x₂))
  -- negTest' (end x) = Unit

  exec {A} (input x x₂) = transport⁻ x₂
  exec {A} (withdraw x) = transport x
  exec {A} (deposit x) = transport x
  exec {A} (x >>= x₂) x₁ =
    let w = exec x (fst x₁)
    in exec _ (snd x₁)
  -- exec {A} (end x) = transport⁻ x 

  -- negTest : G Empty → Type₀
  -- negTest (input x x₁) = {!!}
  -- negTest (withdraw x) = {!!}
  -- negTest (deposit x) = {!!}
  -- negTest (x >>= x₁) = {!!}
  -- negTest (end x) = {!!}

module Shallow {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}} where
  
  -- open AST Identifier prop-mode

  open PropMode one 

  -- open AST Identifier one


  module Shallow (ptps : List Identifier) where
    open AST.InteractionHead {{IsDiscrete-Identifier}} {one} (AST.interactionHead ptps []) 
    
    data G (A : GType) : Type₀ where
      pure : ∀ {sc} → (e : Expr (con [] sc) A) → ⟨ IsPureE e ⟩ → G A
      input : String → G A
      -- withdraw : 
      _>>_ : ∀ {A'} → G A' → G A → G A 

    -- _>>=_ : ∀ {A sc Τ nm} → Expr (con [] sc) Τ
    --                      → Expr (con [ AST.ice sc nm Τ ] sc) A → G A
    -- _>>=_ = {!!}


{-# OPTIONS --cubical  #-}
module Glow.Simple.Trace where

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

open import Glow.Simple.VarSubst

open import Glow.Simple.Monad


open import Cubical.HITs.Interval



module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}} where


  module Trave {ptps : List Identifier} (ce : AST.ContextEntry {prop-mode = one} (AST.interactionHead ptps []) ) where

    open AST.InteractionHead  {prop-mode = one} (AST.interactionHead ptps []) 

    open SubstAll {Identifier} {ptps}
    open SubstOne {Identifier} {ptps}


    {-# TERMINATING #-}
    Trace : ∀ sc → Statements (con [] sc) → Type₀
    TraceNBS : ∀ sc → NBStmnt (con [] sc) → Σ Type₀ λ Ty → Ty → Type₀ → Type₀

    TraceE : ∀ sc → ∀ {Τ} → (e : Expr (con [] sc) Τ) → IsEmpty ⟨ IsPureE e ⟩
                                  → Σ Type₀ λ Tr → (Tr → (Maybe (GTypeAgdaRep Τ)))


    Trace sc []L = Unit
    Trace sc ss@(bindingS (BS-let ce x₁) ∷L x) with proj₁ (snd (IsPureE x₁))
    ... | yes p =
          let v = evalPureExpr x₁ p
              e' = substOneStmnts (inl v) (mkStatements* x) 
          in Trace sc e'

    ... | no ¬p = Σ (fst (TraceE _ x₁ ¬p))
                ((recMaybe Unit λ v → Trace _ (substOneStmnts (inl v) (mkStatements* x))) ∘ snd (TraceE _ x₁ ¬p))
       
    Trace sc ss@(AST.bindingS (AST.BS-publish! p (AST.psof name {()})) ∷L x)
    
    Trace sc ss@(AST.nonBindingS (AST.stmntNBS x₁) ∷L x) =
      Σ (fst (TraceNBS sc x₁)) (λ x₂ → (snd (TraceNBS sc x₁)) x₂ (Trace sc x))
    Trace sc ss@(AST.nonBindingS (AST.exprNBS {Τ} x₁) ∷L x)  with proj₁ (snd (IsPureE x₁))
    ... | yes p = Trace sc x


    ... | no ¬p = Σ (fst (TraceE _ x₁ ¬p))
                    (caseMaybe {A = GTypeAgdaRep Τ} Unit (Trace _ x) ∘ (snd (TraceE _ x₁ ¬p)))
                     -- ((caseMaybe {A = GTypeAgdaRep Τ} Unit (Trace _ x)))

        -- Σ (fst (TraceE _ x₁ ¬p))
        --         ((recMaybe Unit λ v → Trace _ (substOneStmnts (inl v) (mkStatements* x))) ∘ snd (TraceE _ x₁ ¬p))

      


    TraceNBS sc _ = 𝟚 , λ x x₁ → Cubical.Data.Bool.if x then x₁ else Unit

    -- TraceNBS sc (AST.NBS-require! x) = 𝟚 , {!!}
    -- TraceNBS sc (AST.NBS-deposit! x x₁) = 𝟚 , {!!}
    -- TraceNBS sc (AST.NBS-withdraw! x x₁) = 𝟚 , {!!}

    TraceE sc (AST.var (AST.dsot name {inr (x₁ , ())})) x
    TraceE sc (AST.body (AST.bodyR stmnts₁ expr₁)) x = {!x!}
    TraceE sc (AST.lit x₁) x = empty-elim (x tt)
    TraceE sc {Τ} (AST.input x₁) x = Maybe (GTypeAgdaRep Τ) , idfun _
    TraceE sc (AST.if e then e₁ else e₂) x = {!!}

      -- dec-rec _ {{proj₁ (snd (IsPureStmnts ss))}}
      --   (λ x₁ → {!!})
      --   (λ x₁ → {!!})


-- data G (A : Type₀) : Type₁ where
--   input : ∀ {A'} → String → {{IsGlowTy A'}} → A ≡ Maybe A'  → G A
--   withdraw : 𝟚 ≡ A → G A
--   deposit : 𝟚 ≡ A → G A
--   _>>=_ : ∀ {A'} → G A' → (A' → G A) → G A
--   -- end : A ≡ Unit → G A
  
-- input' : String → (Τ : GType) → G (Maybe (GTypeAgdaRep Τ)) 
-- input' x Τ = input x {{GTypeAgdaRep' Τ}} refl

-- -- _>>=_ : ∀ {A B : Type₀} → G A → (A → G B) → G B
-- -- _>>=_ = {!!}

-- _>>_ : ∀ {A B : Type₀} → G A → G B → G B
-- x >> x₁ = x >>= const x₁


-- -- doTest : G {!!}
-- -- doTest = do
-- --    z ← input' "xxx" Bool
-- --    g z
-- negTest' : ∀ {A} → G A → Type₀


-- exec : ∀ {A} → (x : G A) → (negTest' x) → A

-- negTest' (input {A'} x x₁) = Maybe A'
-- negTest' (withdraw x) = 𝟚
-- negTest' (deposit x) = 𝟚
-- negTest' (x >>= x₁) = Σ (negTest' x) λ x₂ → negTest' (x₁ (exec x x₂))
-- -- negTest' (end x) = Unit

-- exec {A} (input x x₂) = transport⁻ x₂
-- exec {A} (withdraw x) = transport x
-- exec {A} (deposit x) = transport x
-- exec {A} (x >>= x₂) x₁ =
--   let w = exec x (fst x₁)
--   in exec _ (snd x₁)
-- -- exec {A} (end x) = transport⁻ x 

-- -- negTest : G Empty → Type₀
-- -- negTest (input x x₁) = {!!}
-- -- negTest (withdraw x) = {!!}
-- -- negTest (deposit x) = {!!}
-- -- negTest (x >>= x₁) = {!!}
-- -- negTest (end x) = {!!}


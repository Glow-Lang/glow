

{-# OPTIONS --cubical --no-import-sorts #-}
module Glow.Chain where

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


module Chains {ℓn ℓe} (Node : Type ℓn) (node₀ : Node)
                    (Edge : Node → Type ℓe)
                    (target : ∀ n → Edge n → Node) where

 Chain : ℕ → Node → Type (ℓ-max ℓn ℓe)

 -- lastNode : ∀ {n} → Chain n → Node
 
 record Link (n : ℕ) (end : Node) : Type (ℓ-max ℓn ℓe) where
   constructor link
   inductive
   field
    preEnd : Node
    tail : Chain n preEnd
    linkE : Edge preEnd
    link≡ : target _ linkE ≡ end

 open Link public


 Chain zero x = Lift {j = ℓe} (x ≡ node₀)
 Chain (suc n) x = Link n x

 toStateList : ∀ {n} → ∀ {x} → Chain n x → List (Σ _ Edge)
 toStateList {zero} x₁ = []
 toStateList {suc n} x₁ =  (preEnd x₁ , linkE x₁) ∷ toStateList (tail x₁)

 ChainΣΣ : Type (ℓ-max ℓn ℓe) 
 ChainΣΣ = Σ ℕ λ x → Σ Node (Chain x)

 _c∷_ : ∀ {n} → ∀ {s} → (e : Edge s) → Chain n s → Chain (suc n) (target _ e) 
 e c∷ x = link _ x e refl

 DecidableEdges : Type (ℓ-max ℓn ℓe)
 DecidableEdges = (∀ node → IsEmpty (Edge node) ⊎ (Edge node))

 buildChainUpTo : DecidableEdges → ℕ → ChainΣΣ 
 buildChainUpTo f zero = zero , node₀ , lift refl
 buildChainUpTo f (suc k) =
   let (m , (s , y)) = buildChainUpTo f k

   in Cubical.Data.Sum.elim
        (λ a → _ , _ , y)
        (λ b → _ , _ , b c∷ y)
        (f s)


 FinitePaths : DecidableEdges → Type ℓe 
 FinitePaths de = Σ ℕ (IsEmpty ∘ Edge ∘ fst ∘ snd ∘ (buildChainUpTo de))


module test1 where

  open Chains ℕ zero (const Unit) (λ n x → suc n)

  -- chain-from-0-to-k

  test-0-k : ∀ k → Chain k k
  test-0-k zero = lift refl
  test-0-k (suc k) = tt c∷ (test-0-k k)


module test2 where

  Dirs : ℕ → Type₀
  Dirs zero = Unit
  Dirs (suc x) = Bool

  edgeF : ∀ n → Dirs n → ℕ
  edgeF zero x = suc zero
  edgeF (suc n) false = n
  edgeF (suc n) true = suc (suc n)

  open Chains ℕ zero (Dirs) edgeF 

  alwaysUp : ∀ x → Dirs x  
  alwaysUp zero = tt
  alwaysUp (suc x) = true

  up : ∀ k → Chain k k
  up zero = lift refl
  up (suc zero) =
     Chains.link
      zero
      (lift refl)
      tt
      refl
      
  up (suc (suc k)) =
     Chains.link
      (suc k)
      (up (suc k))
      true
      refl

  upOne : ∀ k → Chain k k → Chain (suc k) (suc k)
  upOne zero x = Chains.link zero (lift refl) tt refl
  upOne (suc k) x = Chains.link (suc k) x true refl


  up-bck : ∀ k → Chain k k → Chain (suc (suc k)) k
  up-bck k x =
    Chains.link
     (suc k)
     (upOne _ x)
     false
     refl


-- --   test-k-0 : ∀ k → point k ≡ point 0
-- --   test-k-0 zero = refl
-- --   test-k-0 (suc zero) = edge false
-- --   test-k-0 (suc (suc k)) = edge false ∙ test-k-0 (suc k) 

-- --   peak : (c₀ c₁ : Chain) → c₀ ≡ c₁ → ℕ
-- --   peak c₀ c₁ x = {!c₀ c₁!}

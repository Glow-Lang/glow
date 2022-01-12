
{-# OPTIONS --cubical  #-}
module Glow.Linked where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List


open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool renaming (if_then_else_ to if_then_else'_)

open import Cubical.Data.Nat.Order.Recursive
open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)



data Linked {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) (c : C) : Type₀

foldLinked : {C : Type₀} {A : C → Type₀} {fld : ∀ c → A c → C} {c : C}
                → Linked fld c → C

data Linked {C} {A} fld c  where
  L[] : Linked fld c
  _L∷_ : (x : Linked fld c) → A (foldLinked x) → Linked fld c

foldLinked {c = c} L[] = c
foldLinked {fld = fld} (x L∷ x₁) = fld (foldLinked x) x₁

-- TODO : remove unsafe pragma by stratifing on lengths
-- {-# TERMINATING #-}
-- linkedUnshift : {C : Type₀} {A : C → Type₀} {fld : ∀ c → A c → C} {c : C}
--               → Linked fld c → Maybe (Σ (A c) (λ x → Linked fld (fld _ x)))
-- linkedUnshift L[] = nothing
-- linkedUnshift (L[] L∷ x₁) = just ( x₁ , L[] )
-- linkedUnshift ((xs L∷ y) L∷ x) =
--   let z = (linkedUnshift xs)
--   in recMaybe
--       (just ({!!} , {!!}))
--       {!!}
--        z


data Linked' {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) (c : C) : Type₀


data Linked' {C} {A} fld c  where
  []L : Linked' fld c
  _∷L_ : (h : A c) → (Linked' fld (fld _ h)) → Linked' fld c

substLinked' : {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) → {c₀ c₁ : C} → c₀ ≡ c₁ → Linked' fld c₀ → Linked' fld c₁ 
substLinked' fld p []L = []L
substLinked' {A = A} fld p (h ∷L x) =
  let z : PathP (λ z → A (p z)) h (subst A p h)
      z = λ z → transp (λ i → A (p (z ∧ i))) (~ z) h
  in subst _ p h  ∷L substLinked' fld (λ i → fld (p i) (z i)) x

foldLinked' : {C : Type₀} {A : C → Type₀} {fld : ∀ c → A c → C} {c : C}
                → Linked' fld c → C
foldLinked' {c = c} []L = c
foldLinked' (h ∷L x) = foldLinked' x


map-Linked'-map : {C C' : Type₀} { A : C → Type₀} {A' : C' → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
                   (g : C → C') → (f :  ∀ {c} → A c → A' (g c) ) → (∀ {c} → {x : A c} → g (fld _ x) ≡ (fld' _ (f x)) ) → {c : C} → Linked' fld c → Linked' fld' (g c) 
map-Linked'-map g f e []L = []L
map-Linked'-map {C = C} {C' = C'} {fld = fld} {fld' = fld'} g f e {c} (h ∷L x) =
  let t = map-Linked'-map {C = C} {C' = C'} {fld = fld} {fld' = fld'} g f e x 
  in f h ∷L
         -- substLinked' fld' e t 
      subst (Linked' fld') e t


map-Linked'-map-fold : {C C' : Type₀} { A : C → Type₀} {A' : C' → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
                   (g : C → C') → (f :  ∀ {c} → A c → A' (g c) ) → (e : ∀ {c} → {x : A c} → g (fld _ x) ≡ (fld' _ (f x)) ) → {c : C}
                    → (l : Linked' fld c) → g (foldLinked' l) ≡  foldLinked' {C'} {A'} {fld'} (map-Linked'-map g f e l) 
map-Linked'-map-fold g f e []L = refl
map-Linked'-map-fold {fld = fld} {fld' = fld'} g f e {c} (h ∷L l) =
  map-Linked'-map-fold {fld' = fld'} g f e l ∙
      λ i → foldLinked' (transp (λ i₁ → Linked' fld' (e {c } {h} (i ∧ i₁ ))) (~ i) (map-Linked'-map g f e l)) --λ i → {!!}

-- -- TODO : remove unsafe pragma by stratifing on lengths
-- module _ {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) where

--     open Iso
--     -- TODO : remove unsafe pragma by stratifing on lengths  
--     {-# TERMINATING #-}   
--     h : ∀ c → Iso (Linked fld c) (Linked' fld c)

--     postulate iposibleCase : ∀ c → (Linked' fld c)

--     fun (h c) L[] = []L
--     fun (h c) (L[] L∷ x₁) = x₁ ∷L []L
--     fun (h c) (xls@(_ L∷ _) L∷ x₁) with fun (h c) xls
--     ... | []L = iposibleCase _
--     ... | y ∷L w = y ∷L fun (h _) (inv (h _) w L∷ {!x₁!})
       
--     inv (h c) = {!!}
--     rightInv (h c) = {!!}
--     leftInv (h c) = {!!}


-- -- {-# TERMINATING #-}
-- -- Iso-Linked-Linked' : {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) (c : C) → Iso (Linked fld c) (Linked' fld c)
-- -- Iso-Linked-Linked' fld = h
-- --   where
-- --     open Iso
-- --     -- TODO : remove unsafe pragma by stratifing on lengths  
-- --     {-# TERMINATING #-}   
-- --     h : ∀ c → Iso (Linked fld c) (Linked' fld c)
    
-- --     fun (h c) L[] = []L
-- --     fun (h c) (L[] L∷ x₁) = x₁ ∷L []L
-- --     fun (h c) (xls@(x L∷ x₂) L∷ x₁) with fun (h c) xls
-- --     ... | w = {!!}
       
-- --     inv (h c) = {!!}
-- --     rightInv (h c) = {!!}
-- --     leftInv (h c) = {!!}


-- -- -- Iso.fun (Iso-Linked-Linked' fld c) L[] = []L
-- -- -- Iso.fun (Iso-Linked-Linked' fld c) (L[] L∷ x₁) =  x₁ ∷L []L
-- -- -- Iso.fun (Iso-Linked-Linked' fld c) (lxs@(x L∷ x₂) L∷ x₁) =
-- -- --   {!Iso.!}
-- -- -- Iso.inv (Iso-Linked-Linked' fld c) = {!!}
-- -- -- Iso.rightInv (Iso-Linked-Linked' fld c) = {!!}
-- -- -- Iso.leftInv (Iso-Linked-Linked' fld c) = {!!}


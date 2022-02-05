
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
open import Cubical.Data.Empty


open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool renaming (if_then_else_ to if_then_else'_)

open import Cubical.Data.Nat.Order.Recursive

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Cubical.Relation.Binary



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

Linked'-F : {C : Type₀} {A : C → Type₀} 
                (F : Type₀ → Type₀) → (∀ c → (A c → C) → F (A c) → C)
                → (fld : ∀ c → A c → C) (c : C) → Type₀
Linked'-F {C} {A} F g fld c  =
   Linked' {C} {F ∘ A} (λ c₁ → g c₁ (fld c₁)) c 

Linked'-Maybe : {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) (c : C) → Type₀
Linked'-Maybe {C} = Linked'-F Maybe λ c → recMaybe {B = C} c 

toLinked'-Maybe : {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) (c : C)
                      → Linked' fld c → Linked'-Maybe fld c
toLinked'-Maybe {C} {A} fld c []L = []L
toLinked'-Maybe {C} {A} fld c (h ∷L x) = (just h ∷L toLinked'-Maybe _ _ x)

fromLinked'-Maybe : {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) (c : C)
                       → Linked'-Maybe fld c → Linked' fld c
fromLinked'-Maybe {C} {A} fld c []L = []L
fromLinked'-Maybe {C} {A} fld c (nothing ∷L x) = fromLinked'-Maybe {C} {A} fld c x
fromLinked'-Maybe {C} {A} fld c (just x₁ ∷L x) = x₁ ∷L fromLinked'-Maybe {C} {A} _ _ x 


Linked'-len : {C : Type₀} {A : C → Type₀} {fld : ∀ c → A c → C} {c : C} → Linked' fld c → ℕ
Linked'-len []L = zero
Linked'-len (h ∷L x) = suc (Linked'-len x)

Linked'-of-len : {C : Type₀} {A : C → Type₀} {fld : ∀ c → A c → C} {c : C} → ℕ → Linked' fld c → Type₀
Linked'-of-len zero []L = Unit
Linked'-of-len zero (h ∷L x₁) = ⊥
Linked'-of-len (suc x) []L = ⊥
Linked'-of-len (suc x) (h ∷L x₁) = Linked'-of-len x x₁

Linked'-of-len< : {C : Type₀} {A : C → Type₀} {fld : ∀ c → A c → C} {c : C} → ℕ → Linked' fld c → Type₀
Linked'-of-len< zero _ = ⊥
Linked'-of-len< (suc x) []L = Unit
Linked'-of-len< (suc x) (h ∷L x₁) = Linked'-of-len< x x₁

Linked'-of-len<-len : {C : Type₀} {A : C → Type₀} {fld : ∀ c → A c → C} {c : C} → (l : Linked' fld c) → Linked'-of-len< (suc (Linked'-len l)) l
Linked'-of-len<-len []L = tt
Linked'-of-len<-len (h ∷L l) = Linked'-of-len<-len l

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
                   (g : C → C') → (f :  ∀ {c} → A c → A' (g c) ) → (∀ c → (x : A c) → g (fld _ x) ≡ (fld' _ (f x)) ) → {c : C} → Linked' fld c → Linked' fld' (g c) 
map-Linked'-map g f e []L = []L
map-Linked'-map {C = C} {C' = C'} {fld = fld} {fld' = fld'} g f e {c} (h ∷L x) =
  let t = map-Linked'-map {C = C} {C' = C'} {fld = fld} {fld' = fld'} g f e x 
  in f h ∷L
         -- substLinked' fld' e t 
      subst (Linked' fld') (e _ _) t


-- map-Linked'-map-Rel : {C C' : Type₀} { A : C → Type₀} {A' : C' → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
--                     → EquivRel C ℓ-zero
--                    → (g : C → C') → (f :  ∀ {c} → A c → A' (g c) ) → (∀ c → (x : A c) → g (fld _ x) ≡ (fld' _ (f x)) ) → {c : C} → Linked' fld c → Linked' fld' (g c) 
-- map-Linked'-map-Rel = {!!}


map-Linked'-map-fold : {C C' : Type₀} { A : C → Type₀} {A' : C' → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
                   (g : C → C') → (f :  ∀ {c} → A c → A' (g c) ) → (e : ∀ c → (x : A c) → g (fld _ x) ≡ (fld' _ (f x)) ) → {c : C}
                    → (l : Linked' fld c) → g (foldLinked' l) ≡  foldLinked' {C'} {A'} {fld'} (map-Linked'-map g f e l) 
map-Linked'-map-fold g f e []L = refl
map-Linked'-map-fold {fld = fld} {fld' = fld'} g f e {c} (h ∷L l) =
  map-Linked'-map-fold {fld' = fld'} g f e l ∙   
        -- {!!}
      λ i → foldLinked' (transp (λ i₁ → Linked' fld' (e c h (i ∧ i₁ ))) (~ i) (map-Linked'-map g f e l)) --λ i → {!!}



map-Linked'-map-bck-strt : ∀ n → {C C' : Type₀} { A : C → Type₀} {A' : C' → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
                   (g : C' → C) → (f :  ∀ {c'} → A (g c') → A' c' ) → (∀ {c'} → {x : A (g c')} → (fld _ x) ≡ g (fld' _ (f x)) )
                    → {c' : C'} → (l : Linked' fld (g c')) → Linked'-of-len< n l →  Linked' fld' c' 
map-Linked'-map-bck-strt n g f e []L _ = []L
map-Linked'-map-bck-strt (suc n) {C = C} {C' = C'} {fld = fld} {fld' = fld'} g f e {c} (h ∷L x) q =
  let (x' , q') = subst (λ x₁ → Σ (Linked' fld x₁) (Linked'-of-len< n) ) e (x , q)
     
  in f h ∷L map-Linked'-map-bck-strt n {C = C} {C' = C'} {fld = fld} {fld' = fld'} g f e x' q'

map-Linked'-map-bck : {C C' : Type₀} { A : C → Type₀} {A' : C' → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
                   (g : C' → C) → (f :  ∀ {c'} → A (g c') → A' c' ) → (∀ {c'} → {x : A (g c')} → (fld _ x) ≡ g (fld' _ (f x)) )
                    → {c' : C'} → Linked' fld (g c') →  Linked' fld' c' 
map-Linked'-map-bck g f e {c} l = 
  map-Linked'-map-bck-strt ((suc (Linked'-len l))) g f e l
    (Linked'-of-len<-len l)


map-Linked'-map-fold-bck-strt : ∀ n → {C C' : Type₀} { A : C → Type₀} {A' : C' → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
                     (g : C' → C) → (f :  ∀ {c'} → A (g c') → A' c' ) → (e : ∀ {c'} → {x : A (g c')} → (fld _ x) ≡ g (fld' _ (f x)) )
                    → {c' : C'} → (l : Linked' fld (g c')) → ∀ q →  foldLinked' l ≡  g (foldLinked' {C'} {A'} {fld'} (map-Linked'-map-bck-strt n g f e l q)) 
map-Linked'-map-fold-bck-strt _ g f e []L _ = refl
map-Linked'-map-fold-bck-strt (suc n) {C} {C'} {A} {A'} {fld = fld} {fld' = fld'} g f e {c} (h ∷L l) q =
  let (l' , q') = subst (λ x₁ → Σ (Linked' fld x₁) (Linked'-of-len< n) ) e (l , q)

      z = map-Linked'-map-fold-bck-strt n {fld = fld} {fld' = fld'} g f e l' q'
  in  (λ i → foldLinked' (subst-filler (Linked' fld) e l i)) ∙ z


map-Linked'-map-fold-bck : {C C' : Type₀} { A : C → Type₀} {A' : C' → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
                     (g : C' → C) → (f :  ∀ {c'} → A (g c') → A' c' ) → (e : ∀ {c'} → {x : A (g c')} → (fld _ x) ≡ g (fld' _ (f x)) )
                    → {c' : C'} → (l : Linked' fld (g c')) → foldLinked' l ≡  g (foldLinked' {C'} {A'} {fld'} (map-Linked'-map-bck g f e l)) 
map-Linked'-map-fold-bck g f e l = 
    map-Linked'-map-fold-bck-strt ((suc (Linked'-len l))) g f e l
    (Linked'-of-len<-len l)


--------- filter

filter-Linked' : {C : Type₀} { A : C → Type₀} {fld : ∀ c → A c → C}
                   (f :  ∀ {c} → (x : A c) →  Maybe ((Linked' fld (fld c x) → Linked' fld c)))
                   → {c : C} → Linked' fld c → Linked' fld c

filter-Linked' f []L = []L
filter-Linked' f (h ∷L x) = 
   recMaybe ( (h ∷L filter-Linked' f x))
             (λ y → y x)
             (f h)



--------- filterMap

filter-map-Linked' : {C C' : Type₀} {A : C → Type₀} {A' : C' → Type₀}
                                    {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
                                    (g : C → C')
                                  → (f :  ∀ {c} → A c → Maybe (A' (g c)) )
                                  → ((c : C) (x : A c) → g (fld c x) ≡ recMaybe (g c) (fld' (g c)) (f x))
                   → {c : C} → Linked' fld c → Linked' fld' (g c)

filter-map-Linked' {C} {C'} g f x =
   fromLinked'-Maybe _ _ ∘ (map-Linked'-map g f x)



--- collect sigma

Linked'-collect-Σ : {C : Type₀}
                     {D : C → Type₀}
                     { A : C → Type₀} 
                        {fld : ∀ c → A c → C}
                        {fld-D : ∀ c → (a : A c) → D c →  D (fld c a)}
                   → {c : C} → (d : D c)
                   → Linked' fld c → Linked' {Σ _ D} ((λ c₁ x₁ → fld (fst c₁) x₁ , fld-D (fst c₁) x₁ (snd c₁))) (c , d)
Linked'-collect-Σ d []L = []L
Linked'-collect-Σ d (h ∷L x) = h ∷L (Linked'-collect-Σ _ x)
----- map linked sigma

map-Linked'-map-Σ : {C C' : Type₀}
                     {D : C → Type₀}
                     { A : C → Type₀} {A' : C' → Type₀}
                        {fld : ∀ c → A c → C}
                        {fld-D : ∀ c → (a : A c) → D c →  D (fld c a)}
                        {fld' : ∀ c → A' c → C'}
                   (g : ∀ c → D c → C') → (f :  ∀ {c} → ∀ d → A c → A' (g c d) )
                    → (∀ c → ∀ d → (x : A c) → g (fld c x) (fld-D c x d) ≡ fld' (g c d) (f d x))
                   → {c : C} → ∀ d
                   → Linked' fld c → Linked' fld' (g c d)
                  
map-Linked'-map-Σ {C' = C'} {D = D} {A} {A'} {fld = fld} {fld-D} {fld' = fld'} g f x d =
              map-Linked'-map {Σ _ D} {C'} {A ∘ fst} {A'}
                        {fld = λ c₁ x₁ → fld (fst c₁) x₁ , fld-D (fst c₁) x₁ (snd c₁)}
                        {fld' = fld'} 
                       (λ x₁ → g _ (snd x₁)) (λ {y} → f (snd y)) (λ d → x (fst d) (snd d)) {_ , d}
            ∘  Linked'-collect-Σ d

map-Linked'-map-Σ-Mb : {C C' : Type₀}
                     {D : C → Type₀}
                     { A : C → Type₀} {A' : C' → Type₀}
                        {fld : ∀ c → A c → C}
                        (fld-D : ∀ c → (a : A c) → D c →  D (fld c a))
                        {fld' : ∀ c → A' c → C'}
                   (g : ∀ c → D c → C')
                    → (f :  ∀ {c} → ∀ d → A c → Maybe (A' (g c d)) )
                    → ((c : C) (d : D c) (x : A c) →
                         g (fld c x) (fld-D c x d) ≡ recMaybe (g c d) (fld' (g c d)) (f d x))
                   → {c : C} → ∀ d
                   → Linked' fld c → Linked' fld' (g c d)
                  
map-Linked'-map-Σ-Mb {C' = C'} {D = D} {A} {A'} {fld = fld} fld-D {fld' = fld'} g f x d =
     fromLinked'-Maybe _ _ ∘ map-Linked'-map-Σ g f x d







-- -- -- TODO : remove unsafe pragma by stratifing on lengths
-- -- module _ {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) where

-- --     open Iso
-- --     -- TODO : remove unsafe pragma by stratifing on lengths  
-- --     {-# TERMINATING #-}   
-- --     h : ∀ c → Iso (Linked fld c) (Linked' fld c)

-- --     postulate iposibleCase : ∀ c → (Linked' fld c)

-- --     fun (h c) L[] = []L
-- --     fun (h c) (L[] L∷ x₁) = x₁ ∷L []L
-- --     fun (h c) (xls@(_ L∷ _) L∷ x₁) with fun (h c) xls
-- --     ... | []L = iposibleCase _
-- --     ... | y ∷L w = y ∷L fun (h _) (inv (h _) w L∷ {!x₁!})
       
-- --     inv (h c) = {!!}
-- --     rightInv (h c) = {!!}
-- --     leftInv (h c) = {!!}


-- -- -- {-# TERMINATING #-}
-- -- -- Iso-Linked-Linked' : {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) (c : C) → Iso (Linked fld c) (Linked' fld c)
-- -- -- Iso-Linked-Linked' fld = h
-- -- --   where
-- -- --     open Iso
-- -- --     -- TODO : remove unsafe pragma by stratifing on lengths  
-- -- --     {-# TERMINATING #-}   
-- -- --     h : ∀ c → Iso (Linked fld c) (Linked' fld c)
    
-- -- --     fun (h c) L[] = []L
-- -- --     fun (h c) (L[] L∷ x₁) = x₁ ∷L []L
-- -- --     fun (h c) (xls@(x L∷ x₂) L∷ x₁) with fun (h c) xls
-- -- --     ... | w = {!!}
       
-- -- --     inv (h c) = {!!}
-- -- --     rightInv (h c) = {!!}
-- -- --     leftInv (h c) = {!!}


-- -- -- -- Iso.fun (Iso-Linked-Linked' fld c) L[] = []L
-- -- -- -- Iso.fun (Iso-Linked-Linked' fld c) (L[] L∷ x₁) =  x₁ ∷L []L
-- -- -- -- Iso.fun (Iso-Linked-Linked' fld c) (lxs@(x L∷ x₂) L∷ x₁) =
-- -- -- --   {!Iso.!}
-- -- -- -- Iso.inv (Iso-Linked-Linked' fld c) = {!!}
-- -- -- -- Iso.rightInv (Iso-Linked-Linked' fld c) = {!!}
-- -- -- -- Iso.leftInv (Iso-Linked-Linked' fld c) = {!!}



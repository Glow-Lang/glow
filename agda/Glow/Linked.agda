
{-# OPTIONS --cubical  #-}
module Glow.Linked where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sigma renaming (_×_ to _Σ×_)
open import Cubical.Data.Sum renaming (rec to sum-rec ; elim to sum-elim)
open import Cubical.Data.List
open import Cubical.Data.Empty
open import Cubical.Data.Unit


open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool renaming (if_then_else_ to if_then_else'_)

open import Cubical.Data.Nat.Order.Recursive

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Cubical.Relation.Binary

open import Cubical.Categories.Category
open import Cubical.Categories.Functor

open import Cubical.Foundations.CartesianKanOps

-- open import Glow.CategoriesMore


isSet-cong-subst : ∀ {ℓ ℓ′} {A : Type ℓ} (B : A → Type ℓ′)
                → (isSet-A : isSet A)
                → ∀ {a a' : A}
                → (p q : a ≡ a') → (x : B a) → subst B p x ≡ subst B q x
isSet-cong-subst B isSet-A p q x = cong (λ a → subst B a x) (isSet-A _ _ _ _)

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


--------


map-Linked'-map'-Mb* : {C C' : Type₀}
                     { A : C → Type₀} {A' : C' → Type₀}
                        {fld : ∀ c → A c → C}
                        {fld' : ∀ c → A' c → C'}
                   (g : ∀ c → A c →  C')
                    → (f :  ∀ {c} → (a : A c) → ∀ h → (g (fld c a) h ≡ g c a) ⊎ Σ (A' (g c a))
                          λ v → g (fld c a) h ≡ fld' (g c a) v )
                   → {c : C} → (a : A c)
                   → Linked' fld (fld _ a )
                   → Linked' fld' (g c a) 
                  
map-Linked'-map'-Mb* g f a []L = []L
map-Linked'-map'-Mb* {A = A} {fld' = fld'} g f a (h ∷L x₁) =
 let t = map-Linked'-map'-Mb* {A = A} {fld' = fld'} g f h x₁
 in  
   sum-rec
       (λ e → subst (Linked' fld') e t)
       (λ x₂ → fst x₂ ∷L subst (Linked' fld') ((snd x₂)) t )
       (f a h)



Linked'-head : ∀ {C : Type₀} {A : C → Type₀} {fld} {c : C} → Linked' {A = A} fld c
                                  → Maybe (A c)  
Linked'-head []L = nothing
Linked'-head (h ∷L x) = just h

map-Linked'-map'-Mb : {C C' : Type₀}
                     { A : C → Type₀} {A' : C' → Type₀}
                        {fld : ∀ c → A c → C}
                        {fld' : ∀ c → A' c → C'}
                   (g₀ : C → C')
                   (g : ∀ c → A c →  C')
                    → (f :  ∀ {c} → (a : A c) → ∀ h → (g (fld c a) h ≡ g c a) ⊎ Σ (A' (g c a))
                          λ v → g (fld c a) h ≡ fld' (g c a) v )
                   → {c : C} 
                   → (x : Linked' fld c) 
                   → Linked' fld' (recMaybe (g₀ c) (g _) (Linked'-head x)) 
                  
map-Linked'-map'-Mb g₀ g f []L = []L
map-Linked'-map'-Mb g₀ g f (h ∷L x) = map-Linked'-map'-Mb* g f h x



module LinkedPathHLevel {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) where
  Li = Linked' fld

  Cover : ∀ c → Li c → Li c → Type₀
  Cover c []L []L = Unit
  Cover c []L (h ∷L x₁) = ⊥
  Cover c (h ∷L x) []L = ⊥
  Cover c (h ∷L x) (h₁ ∷L x₁) = Σ (h₁ ≡ h) λ p → Cover (fld _ h) x (subst Li (cong (fld c) p) x₁)
     --λ p → PathP (λ i → Li (fld _ (p i))) x x₁

  reflCode : ∀ c → ∀ xs → Cover c xs xs
  reflCode _ []L = tt
  reflCode c (h ∷L xs) = refl , subst (Cover (fld _ h) xs) ((sym (substRefl {B = Li} xs))) (reflCode _ xs)


  encode : ∀ c → ∀ xs ys → (p : xs ≡ ys) → Cover c xs ys
  encode c xs _ = J (λ ys _ → Cover c xs ys) (reflCode c xs)

  encodeRefl : ∀ c xs → encode c xs xs refl ≡ reflCode c xs
  encodeRefl c xs = JRefl (λ ys _ → Cover c xs ys) (reflCode c xs)

  decode : ∀ c → ∀ xs ys → Cover c xs ys → xs ≡ ys
  decode c []L []L x = refl
  decode c (h ∷L xs) (h₁ ∷L ys) x i =
     fst x (~ i) ∷L toPathP {A = λ x₁ → Linked' fld (fld c (fst x (x₁)))} {x = ys}
     (sym (decode _ _ _ (snd x))) 
     (~ i)

  decodeRefl : ∀ c → ∀ xs → decode c xs xs (reflCode c xs) ≡ refl
  decodeRefl _ []L = refl
  decodeRefl c (x ∷L xs) = cong (cong₂ _∷L_ refl) (sq ∙ decodeRefl _ xs )
   where

     -- pp : PathP {!!} ≡ decode (fld c x) xs xs (reflCode (fld c x) xs)
     -- pp = {!!}

     sq : Square (λ i → _)
                     (decode (fld c x) xs xs (reflCode (fld c x) xs))
                     refl refl
     sq i j =
        hcomp (λ l → λ {
             (i = i1) → decodeRefl (fld c x) xs (~ l) j
           ; (j = i1) → xs
           ; (j = i0) → decode (fld c x) xs
                  (transport (λ _ → Li (fld c x)) xs)
                  (transport (λ ii → Cover (fld c x) xs (transportRefl xs (~ ii))) (reflCode (fld c x) xs))
                  ((~ l) ∧ (~ i) )
           }) 
             (hcomp
              ((λ l → λ {
             (i = i0) → transportRefl xs (j ∨ (~ l)) 
           ; (i = i1) → xs
           ; (j = i0) →
                   decode (fld c x) xs (transportRefl xs ((~ l)))
                     ((transp (λ jj → cong (Cover (fld c x) xs) (λ i₁ → transportRefl xs ((~ l) ∨ (~ i₁))) jj) (~ l) (reflCode (fld c x) xs)))
                     (~ i)
           ; (j = i1) → xs
           })) (decodeRefl (fld c x) xs j (~ i)) )

  decodeEncode : ∀ c → ∀ xs ys → (p : xs ≡ ys) → decode c xs ys (encode c xs ys p) ≡ p
  decodeEncode c xs _ =
    J (λ ys p → decode c xs ys (encode c xs ys p) ≡ p)
      (cong (decode c xs xs) (encodeRefl c xs) ∙ decodeRefl c xs)

  isOfHLevelCover : ∀ c → (n : HLevel) (p : ∀ {c'} → isOfHLevel (suc (suc n)) (A c'))
    (xs ys : Li c) → isOfHLevel (suc n) (Cover c xs ys)
  isOfHLevelCover c n _ []L []L = (isProp→isOfHLevelSuc n isPropUnit)
  isOfHLevelCover c n _ []L (h ∷L ys) = (isProp→isOfHLevelSuc n isProp⊥)
  isOfHLevelCover c n _ (h ∷L xs) []L = (isProp→isOfHLevelSuc n isProp⊥)
  isOfHLevelCover c n p (h ∷L xs) (h₁ ∷L ys) = isOfHLevelΣ (suc n) (p h₁ h) λ y → isOfHLevelCover (fld c h) n p xs (subst Li (λ i → fld c (y i)) ys) 

  isOfHLevelLinked : ∀ c → (n : HLevel) 
    → (∀ {c'} → isOfHLevel (suc (suc n)) (A c')) → isOfHLevel (suc (suc n)) (Li c)
  isOfHLevelLinked c n ofLevel xs ys =
    isOfHLevelRetract (suc n)
      (encode c xs ys)
      (decode c xs ys)
      (decodeEncode c xs ys)
      (isOfHLevelCover c n ofLevel xs ys)


-- module FreeCategoryJ {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) where



--   Li = Linked' fld

--   foldLinked'-subst : ∀ {c c'} → (p : c ≡ c') → ∀ x →  foldLinked' x ≡ foldLinked' (subst Li p x)
--   foldLinked'-subst {c} {c'} =
--     J {_} {C} {c} (λ y p → ∀ x → foldLinked' x ≡ foldLinked' (subst Li p x)) λ x →  cong foldLinked' (sym (substRefl {B = Li} x))  


--   _++L_ : ∀ {c₀} → (f : Li c₀) → Li (foldLinked' f) → Li c₀ 
--   []L ++L g = g
--   (h ∷L f) ++L g = h ∷L (f ++L g)

--   ++L-coh : ∀ {c₀} → (f : Li c₀) → (g : Li (foldLinked' f)) → foldLinked' g ≡ foldLinked' (f ++L g) 
--   ++L-coh []L g = refl
--   ++L-coh (h ∷L f) g = ++L-coh f g

--   ++L-rUnit : ∀ {c₀} → (f : Li c₀) → (f ++L []L) ≡ f
--   ++L-rUnit []L = refl
--   ++L-rUnit (h ∷L f) = cong (h ∷L_) (++L-rUnit f)


--   ++L-assoc : ∀ {c₀} → (f : Li c₀) → (g : Li (foldLinked' f)) → (h : Li (foldLinked' (f ++L g))) → ((f ++L g) ++L h) ≡ (f ++L (g ++L (subst⁻ Li (++L-coh f g) h))) 
--   ++L-assoc []L g h = cong (g ++L_) (sym (substRefl {B = Li} h))
--   ++L-assoc (h₁ ∷L f) g h = cong (h₁ ∷L_) (++L-assoc f g h)


--   -- FreeCategory : Category ℓ-zero ℓ-zero
--   -- Category.ob FreeCategory = C
--   -- Category.Hom[_,_] FreeCategory s t = Σ (Li s) ((t ≡_ ) ∘ foldLinked') 
--   -- Category.id FreeCategory = []L , refl
--   -- Category._⋆_ FreeCategory f g = (fst f ++L subst Li (snd f) (fst g)) , (snd g ∙ foldLinked'-subst (fst g) (snd f)) ∙ ++L-coh (fst f) (subst Li (snd f) (fst g))
--   -- Category.⋆IdL FreeCategory {c₀} {c₁} f = ΣPathP (substRefl {B = Li} (fst f) , toPathP (IdL-coh {c₀} {c₁} (fst f) (snd f)))
--   -- Category.⋆IdR FreeCategory = {!!}
--   -- Category.⋆Assoc FreeCategory = {!!}
--   -- Category.isSetHom FreeCategory = {!!}

--   lem1 : ∀ {c₀ c₁} → (f : Li c₀) → (p : c₀ ≡ c₁) → (g : Li (foldLinked' (subst Li p f)))
--      → (subst Li p f) ++L g ≡ subst Li p (f ++L subst Li (sym (foldLinked'-subst p f)) g)
--   lem1 f p g = {!!}
--   -- lem1 (h ∷L f) p g = {!!}



-- --   -- FreeCategory : isGroupoid C → (∀ c → isSet (A c)) → Category ℓ-zero ℓ-zero
-- --   -- Category.ob (FreeCategory _ _) = C
-- --   -- Category.Hom[_,_] (FreeCategory _ _) s t = Σ (Li s) ((t ≡_ ) ∘ foldLinked') 
-- --   -- Category.id (FreeCategory _ _) = []L , refl
-- --   -- Category._⋆_ (FreeCategory _ _) f g = (fst f ++L subst Li (snd f) (fst g)) , snd g ∙∙ foldLinked'-subst (fst g) (snd f) ∙∙ ++L-coh (fst f) (subst Li (snd f) (fst g))
-- --   -- Category.⋆IdL (FreeCategory isSet-C _) {x₁} {y = y} f = ΣPathP (substRefl {B = Li} _ , {!!})
-- --   --           -- symP (toPathP ({!!}
-- --   --           --   ∙ fromPathP ( (doubleCompPath-filler (snd f) (foldLinked'-subst (fst f) (λ _ → x₁))  (λ _ → foldLinked' (subst Li (λ _ → x₁) (fst f)))))) ))
-- --   -- Category.⋆IdR (FreeCategory isSet-C _) {x₁} {y} f = ΣPathP (++L-rUnit _  , {!symP (doubleCompPath-filler (refl {x = y}) (snd f) (++L-coh (fst f) (subst Li (snd f) []L)))!} )
-- --   --   --        symP (( {!doubleCompPath-filler (refl {x = y}) (snd f) (++L-coh (fst f) (subst Li (snd f) []L))!}))) 
-- --   -- Category.⋆Assoc (FreeCategory isSet-C _) f g h = 
-- --   --    ΣPathP (++L-assoc (fst f) _ _ ∙ cong ((fst f) ++L_)
-- --   --      pp
-- --   --      , toPathP {!!})
-- --   --  where
-- --   --    pp : _
-- --   --    pp = lem1 (fst g) _ _ ∙ {!!} 
-- --   --         -- cong (subst Li (snd f) )
-- --   --         --   (cong (fst g ++L_) (sym (substComposite Li _ _ (fst h) ∙ substComposite Li _ _ (subst Li (
-- --   --         --                                                                                                    ((snd g ∙ foldLinked'-subst (fst g) (snd f)) ∙
-- --   --         --                                                                                                     ++L-coh (fst f) (subst Li (snd f) (fst g)))
-- --   --         --                                                                                                    ) (fst h))) ∙ cong (λ a → subst Li a (fst h)) {!!}))

-- --   -- Category.isSetHom (FreeCategory isSet-C isSet-A) = 
-- --   --   isOfHLevelΣ 2 (LinkedPathHLevel.isOfHLevelLinked fld _ 0 (isSet-A _)) λ x₃ → (isSet-C _ _) --{!LinkedPathHLevel.isOfHLevelLinked!}



--   FreeCategory' : isSet C → (∀ c → isSet (A c)) → Category ℓ-zero ℓ-zero
--   Category.ob (FreeCategory' _ _) = C
--   Category.Hom[_,_] (FreeCategory' _ _) s t = Σ (Li s) ((t ≡_ ) ∘ foldLinked') 
--   Category.id (FreeCategory' _ _) = []L , refl
--   Category._⋆_ (FreeCategory' _ _) f g = (fst f ++L subst Li (snd f) (fst g)) , snd g ∙∙ foldLinked'-subst (snd f) (fst g)  ∙∙ ++L-coh (fst f) (subst Li (snd f) (fst g))
--   Category.⋆IdL (FreeCategory' isSet-C _) _ = Σ≡Prop (λ _ → isSet-C _ _) (substRefl {B = Li} _)
--   Category.⋆IdR (FreeCategory' isSet-C _) f = Σ≡Prop (λ _ → isSet-C _ _) (++L-rUnit _) 
--   Category.⋆Assoc (FreeCategory' isSet-C _) f g h = 
--      Σ≡Prop (λ _ → isSet-C _ _)
--          (++L-assoc (fst f) _ _ ∙ {!pp!})

--      where
--        pp : _
--        pp = lem1 (fst g) _ _ ∙ (cong ((subst Li (snd f)) ∘ (fst g ++L_))
--           (sym (cong (λ a → subst Li a (fst h)) (isSet-C _ _ _ _) ∙∙ substComposite Li (snd g ∙∙ _ ∙∙ _) _ (fst h) ∙∙ substComposite Li _ _ (subst Li _ (fst h))  )))
     

--   Category.isSetHom (FreeCategory' isSet-C isSet-A) =
--     isOfHLevelΣ 2 (LinkedPathHLevel.isOfHLevelLinked fld _ 0 (isSet-A _)) λ x₃ → isProp→isSet (isSet-C _ _) --{!LinkedPathHLevel.isOfHLevelLinked!}


module FreeCategory {C : Type₀} {A : C → Type₀} (fld : ∀ c → A c → C) where



  Li = Linked' fld

  foldLinked'-subst : ∀ {c c'} → ∀ x → (p : c ≡ c')  →  foldLinked' x ≡ foldLinked' (subst Li p x)
  foldLinked'-subst []L p = p
  foldLinked'-subst (_ ∷L x) _ = foldLinked'-subst x _


  _++L_ : ∀ {c₀} → (f : Li c₀) → Li (foldLinked' f) → Li c₀ 
  []L ++L g = g
  (h ∷L f) ++L g = h ∷L (f ++L g)


  ++L-coh : ∀ {c₀} → (f : Li c₀) → (g : Li (foldLinked' f)) → foldLinked' g ≡ foldLinked' (f ++L g) 
  ++L-coh []L g = refl
  ++L-coh (h ∷L f) g = ++L-coh f g

  ++L-rUnit : ∀ {c₀} → (f : Li c₀) → (f ++L []L) ≡ f
  ++L-rUnit []L = refl
  ++L-rUnit (h ∷L f) = cong (h ∷L_) (++L-rUnit f)


  ++L-assoc : ∀ {c₀} → (f : Li c₀) → (g : Li (foldLinked' f)) → (h : Li (foldLinked' (f ++L g))) → ((f ++L g) ++L h) ≡ (f ++L (g ++L (subst⁻ Li (++L-coh f g) h))) 
  ++L-assoc []L g h = cong (g ++L_) (sym (substRefl {B = Li} h))
  ++L-assoc (h₁ ∷L f) g h = cong (h₁ ∷L_) (++L-assoc f g h)


  -- FreeCategory : Category ℓ-zero ℓ-zero
  -- Category.ob FreeCategory = C
  -- Category.Hom[_,_] FreeCategory s t = Σ (Li s) ((t ≡_ ) ∘ foldLinked') 
  -- Category.id FreeCategory = []L , refl
  -- Category._⋆_ FreeCategory f g = (fst f ++L subst Li (snd f) (fst g)) , (snd g ∙ foldLinked'-subst (fst g) (snd f)) ∙ ++L-coh (fst f) (subst Li (snd f) (fst g))
  -- Category.⋆IdL FreeCategory {c₀} {c₁} f = ΣPathP (substRefl {B = Li} (fst f) , toPathP (IdL-coh {c₀} {c₁} (fst f) (snd f)))
  -- Category.⋆IdR FreeCategory = {!!}
  -- Category.⋆Assoc FreeCategory = {!!}
  -- Category.isSetHom FreeCategory = {!!}

  lem1 : ∀ {c₀ c₁} → (f : Li c₀) → (p : c₀ ≡ c₁) → (g : Li (foldLinked' (subst Li p f)))
     → (subst Li p f) ++L g ≡ subst Li p (f ++L subst Li (sym (foldLinked'-subst f p)) g)
  lem1 []L p g = sym (transportTransport⁻ (cong Li p) _)
  lem1 (h ∷L f) p g = cong (_ ∷L_) (lem1 f _ g)



  -- FreeCategory : isGroupoid C → (∀ c → isSet (A c)) → Category ℓ-zero ℓ-zero
  -- Category.ob (FreeCategory _ _) = C
  -- Category.Hom[_,_] (FreeCategory _ _) s t = Σ (Li s) ((t ≡_ ) ∘ foldLinked') 
  -- Category.id (FreeCategory _ _) = []L , refl
  -- Category._⋆_ (FreeCategory _ _) f g = (fst f ++L subst Li (snd f) (fst g)) , snd g ∙∙ foldLinked'-subst (fst g) (snd f) ∙∙ ++L-coh (fst f) (subst Li (snd f) (fst g))
  -- Category.⋆IdL (FreeCategory isSet-C _) {x₁} {y = y} f = ΣPathP (substRefl {B = Li} _ , {!!})
  --           -- symP (toPathP ({!!}
  --           --   ∙ fromPathP ( (doubleCompPath-filler (snd f) (foldLinked'-subst (fst f) (λ _ → x₁))  (λ _ → foldLinked' (subst Li (λ _ → x₁) (fst f)))))) ))
  -- Category.⋆IdR (FreeCategory isSet-C _) {x₁} {y} f = ΣPathP (++L-rUnit _  , {!symP (doubleCompPath-filler (refl {x = y}) (snd f) (++L-coh (fst f) (subst Li (snd f) []L)))!} )
  --   --        symP (( {!doubleCompPath-filler (refl {x = y}) (snd f) (++L-coh (fst f) (subst Li (snd f) []L))!}))) 
  -- Category.⋆Assoc (FreeCategory isSet-C _) f g h = 
  --    ΣPathP (++L-assoc (fst f) _ _ ∙ cong ((fst f) ++L_)
  --      pp
  --      , toPathP {!!})
  --  where
  --    pp : _
  --    pp = lem1 (fst g) _ _ ∙ {!!} 
  --         -- cong (subst Li (snd f) )
  --         --   (cong (fst g ++L_) (sym (substComposite Li _ _ (fst h) ∙ substComposite Li _ _ (subst Li (
  --         --                                                                                                    ((snd g ∙ foldLinked'-subst (fst g) (snd f)) ∙
  --         --                                                                                                     ++L-coh (fst f) (subst Li (snd f) (fst g)))
  --         --                                                                                                    ) (fst h))) ∙ cong (λ a → subst Li a (fst h)) {!!}))

  -- Category.isSetHom (FreeCategory isSet-C isSet-A) = 
  --   isOfHLevelΣ 2 (LinkedPathHLevel.isOfHLevelLinked fld _ 0 (isSet-A _)) λ x₃ → (isSet-C _ _) --{!LinkedPathHLevel.isOfHLevelLinked!}



  FreeCategory' : isSet C → (∀ c → isSet (A c)) → Category ℓ-zero ℓ-zero
  Category.ob (FreeCategory' _ _) = C
  Category.Hom[_,_] (FreeCategory' _ _) s t = Σ (Li s) ((t ≡_ ) ∘ foldLinked') 
  Category.id (FreeCategory' _ _) = []L , refl
  Category._⋆_ (FreeCategory' _ _) f g = (fst f ++L subst Li (snd f) (fst g)) , snd g ∙∙ foldLinked'-subst (fst g) (snd f) ∙∙ ++L-coh (fst f) (subst Li (snd f) (fst g))
  Category.⋆IdL (FreeCategory' isSet-C _) _ = Σ≡Prop (λ _ → isSet-C _ _) (substRefl {B = Li} _)
  Category.⋆IdR (FreeCategory' isSet-C _) f = Σ≡Prop (λ _ → isSet-C _ _) (++L-rUnit _) 
  Category.⋆Assoc (FreeCategory' isSet-C _) f g h =
     Σ≡Prop (λ _ → isSet-C _ _)
         (++L-assoc (fst f) _ _ ∙ cong ((fst f) ++L_) pp)

     where
       pp : _
       pp = lem1 (fst g) _ _ ∙ (cong ((subst Li (snd f)) ∘ (fst g ++L_))
          (sym (isSet-cong-subst Li isSet-C _ _ (fst h) ∙∙ substComposite Li (snd g ∙∙ _ ∙∙ _) _ (fst h) ∙∙ substComposite Li _ _ (subst Li _ (fst h))  )))
     

  Category.isSetHom (FreeCategory' isSet-C isSet-A) =
    isOfHLevelΣ 2 (LinkedPathHLevel.isOfHLevelLinked fld _ 0 (isSet-A _)) λ x₃ → isProp→isSet (isSet-C _ _) --{!LinkedPathHLevel.isOfHLevelLinked!}

FreeCategory' = FreeCategory.FreeCategory'





module FreCatFunct {C C' : Type₀} { A : C → Type₀} {A' : C' → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C'}
                     (isSet-C : isSet C) (isSet-A : ∀ c → isSet (A c))
                     (isSet-C' : isSet C') (isSet-A' : ∀ c' → isSet (A' c'))
                   (g : C → C') (f :  ∀ {c} → A c → A' (g c) ) (coh : ∀ c → (x : A c) → g (fld _ x) ≡ (fld' _ (f x)) )
                    
                      where
  
  map-Linked'-map-fold-++ : 
                       {c cT : C}
                      → (l : Linked' fld c) → (p : cT ≡ foldLinked' l) → (m : Linked' fld cT)
                      →  map-Linked'-map {fld = fld} g f coh (FreeCategory._++L_ fld l (subst (Linked' fld) p m))
                          ≡ FreeCategory._++L_ fld' (map-Linked'-map {fld = fld} g f coh l) (subst (Linked' fld')
                            ( refl ∙∙ cong g p ∙∙ (map-Linked'-map-fold g f coh l))
                            ((map-Linked'-map {fld = fld} g f coh m)))
  map-Linked'-map-fold-++ {c} {cT} []L = 
     J (λ c p → (m : Linked' fld cT) →
                    map-Linked'-map g f coh (subst (Linked' fld) p m) ≡
                    subst (Linked' fld') (refl ∙∙ cong g p ∙∙ refl) (map-Linked'-map g f coh m))
                     w
     where
       w : _
       w m = cong ( map-Linked'-map g f coh ) (transportRefl _) ∙ sym (isSet-subst {B = (Linked' fld')} isSet-C' (refl ∙∙ refl ∙∙ refl) (map-Linked'-map g f coh m))
     
  map-Linked'-map-fold-++ {c = c} (h ∷L l) p m =
    (cong
      ( (f h ∷L_) ∘ (subst Li pp )) (map-Linked'-map-fold-++ l _ m))
         ∙ cong (f h ∷L_)
             (cong (subst Li pp ∘ (map-Linked'-map g f coh l ++L_)) (isSet-cong-subst Li isSet-C' _ _ ll ∙ substComposite Li _ _ ll)
                ∙ sym (lem1 (map-Linked'-map g f coh l) pp _))
     where
       open FreeCategory fld'

       pp = coh c h

       ll = (map-Linked'-map g f coh m)
      


  map-Linked'-map-functor : Functor (FreeCategory' fld isSet-C isSet-A) (FreeCategory' fld' isSet-C' isSet-A')
  Functor.F-ob map-Linked'-map-functor = g
  fst (Functor.F-hom map-Linked'-map-functor (x , _)) = map-Linked'-map g f coh x
  snd (Functor.F-hom map-Linked'-map-functor (x , p)) = cong g p ∙ map-Linked'-map-fold g f coh x
  Functor.F-id map-Linked'-map-functor = Σ≡Prop (λ _ → isSet-C' _ _) refl
  Functor.F-seq map-Linked'-map-functor f g = Σ≡Prop (λ _ → isSet-C' _ _) (map-Linked'-map-fold-++ (fst f) (snd f) (fst g) ) 

module UnnamedRelFree {C : Type₀} { A : C → Type₀} {A' : C → Type₀} {fld : ∀ c → A c → C} {fld' : ∀ c → A' c → C}
                     (isSet-C : isSet C) (isSet-A : ∀ c → isSet (A c)) (isSet-A' : ∀ c' → isSet (A' c'))
                   (g : C → C) (f :  ∀ {c} → A c → A' (g c) ) (coh : ∀ c → (x : A c) → g (fld _ x) ≡ (fld' _ (f x))) where


  

  -- map-Linked'-map-unnamedRel : UnnamedRel (FreeCategory' fld isSet-C isSet-A) (FreeCategory' fld' isSet-C' isSet-A')
  -- map-Linked'-map-unnamedRel =  w
  --   where
  --     w : UnnamedRel (FreeCategory' fld isSet-C isSet-A)
  --           (FreeCategory' fld' isSet-C' isSet-A')
  --     UnnamedRel.ob-≡ w = {!idfun!}
  --     UnnamedRel.wO w = {!!}
  --     UnnamedRel.wC w = {!!}
  --     UnnamedRel.wD w = {!!}







-- map-Linked'-map'-Mb : {C C' : Type₀}
--                      { A : C → Type₀} {A' : C' → Type₀}
--                         {fld : ∀ c → A c → C}
--                         {fld' : ∀ c → A' c → C'}
--                    (g₀ : C → C')
--                    (g : ∀ c → A c →  C')
--                     → (f :  ∀ {c} → (a : A c) → (A' (g c a)) )
--                     → ((c : C) (x : A c) →
--                          {!!})
--                    → {c : C}
--                    → Linked' fld c → Linked' fld' (g₀ c) 
                  
-- map-Linked'-map'-Mb {A = A} gₒ g f x []L = []L
-- map-Linked'-map'-Mb {C} {C'} {A = A} {fld = fld} {fld' = fld'} g₀ g f e (h ∷L t) =
--     let t = map-Linked'-map'-Mb {C = C} {C' = C'} {fld = fld} {fld' = fld'} g₀ g f e t 
--     in f h ∷L 
--       subst (Linked' fld') (e _ h) t
   






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




{-# OPTIONS --cubical  #-}
module Glow.CategoriesMore where

open import Cubical.Foundations.Everything

open import Cubical.Categories.Category

open import Cubical.Categories.Functor


open import Cubical.Categories.Instances.Discrete


private
  variable
    ℓ ℓ' : Level
    -- C D E : Category ℓ ℓ'
open Category hiding (_∘_)


-- record UnnamedRel (C D : Category ℓ ℓ') : Type (ℓ-suc (ℓ-max ℓ ℓ')) where

--   field
--     ob-≡ : Iso (C .ob) (D .ob)
    

--   t = Iso.fun ob-≡

--   field

--     wO : ∀ {x y z} → C [ x , y ] → D [ (t x) , (t z) ] → C .ob   


--     wC : ∀ {x y z} → (f : C [ x , y ]) → (g : D [ t x , t z ]) → C [ z , wO f g ]

--     wD : ∀ {x y z} → (f : C [ x , y ]) → (g : D [ t x , t z ]) → D [ t y , t (wO f g) ]  

--     -- wO-id : ∀ {x z} → (g : D [ t x , t z ]) → z ≡ wO (Category.id C) g

--     -- wD-id : ∀ {x z} → (g : D [ t x , t z ])  → subst (D [ t x ,_]) (cong t (wO-id g)) g ≡ wD (Category.id C) g

--   C* : Category (ℓ-max ℓ ℓ') ℓ'
--   ob C* = Σ (C .ob) λ x → Σ _ (D .Hom[_,_] (t x) ∘ t)
--   Hom[ C* , (A , _) ] (B , _) = C [ A , B ]
--   Category.id C* = Category.id C
--   _⋆_ C* = _⋆_ C
--   ⋆IdL C* = ⋆IdL C
--   ⋆IdR C* = ⋆IdR C
--   ⋆Assoc C* = ⋆Assoc C
--   isSetHom C* = isSetHom C


record UnnamedRel (C D : Category ℓ ℓ') : Type (ℓ-suc (ℓ-max ℓ ℓ')) where

  field
    ob-≡ : Iso (C .ob) (D .ob)
    

  t = Iso.fun ob-≡

  field

    wO : ∀ {x y z} → C [ x , y ] → D [ (t x) , (t z) ] → C .ob   


    wC : ∀ {x y z} → (f : C [ x , y ]) → (g : D [ t x , t z ]) → C [ z , wO f g ]

    wD : ∀ {x y z} → (f : C [ x , y ]) → (g : D [ t x , t z ]) → D [ t y , t (wO f g) ]  

    wO-id : ∀ {x z} → (g : D [ t x , t z ]) → z ≡ wO (Category.id C) g

    wD-id : ∀ {x z} → (g : D [ t x , t z ])  → subst (D [ t x ,_]) (cong t (wO-id g)) g ≡ wD (Category.id C) g

  C* : Category (ℓ-max ℓ ℓ') (ℓ-max ℓ ℓ')
  ob C* = Σ (C .ob) λ x → Σ _ (D .Hom[_,_] (t x) ∘ t)
  Hom[_,_] C* (A , A' , gA) (B , B' , gB ) = Σ (C .Hom[_,_] A B) λ f → Σ (B' ≡ wO f gA) λ p → subst (D [ (Iso.fun ob-≡ B) ,_]) (cong t p) gB ≡ wD f gA
  fst (Category.id C*) = Category.id C
  fst (snd (Category.id C*)) = wO-id _
  snd (snd (Category.id C* {x , z , g})) = wD-id _
  fst ((C* ⋆ (f , _)) (g , _)) = Category._⋆_ C f g
  fst (snd ((_⋆_ C* {X , X' , gX} {Y , Y' , gY} {Z , Z' , gZ} (f , ff , _)) (g , gg , _))) = gg ∙ {! ff!}
  snd (snd ((C* ⋆ f) g)) = {!!}
  ⋆IdL C* = {!!}
  ⋆IdR C* = {!!}
  ⋆Assoc C* = {!!}
  isSetHom C* = {!!}

  -- FF : Functor C* C
  -- Functor.F-ob FF = (fst ∘ snd)
  -- Functor.F-hom FF {(x , x' , g)} {y} f = {!wC f ? !}
  -- Functor.F-id FF = {!!}
  -- Functor.F-seq FF = {!!}








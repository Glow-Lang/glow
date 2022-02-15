{-# OPTIONS --cubical  #-}
module Glow.ListDecProps where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod renaming (map to map-prod)
open import Cubical.Data.Sum renaming (elim to sum-elim ; map to map-sum ; rec to sum-rec)
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


pop : ∀ {ℓ} → {A : Type ℓ} → List A → Maybe A 
pop [] = nothing
pop (x ∷ x₁) = just x

tail : ∀ {ℓ} → {A : Type ℓ} → List A → List A 
tail [] = []
tail (_ ∷ xs) = xs

-- infixr 80 •

-- pattern • x = just x

map-List-∘ : ∀ {ℓ} → {A B C : Type ℓ} → (f : A → B) → (g : B → C) → (l : List A) →  map-List g (map-List f l) ≡ map-List (g ∘ f) l 
map-List-∘ f g [] = refl
map-List-∘ f g (x ∷ l) = cong ((g (f x)) ∷_) (map-List-∘ f g l)

-- -- usefull for stratification
-- list-< : \al


ForAllMember : ∀ {ℓ ℓ'} → {A : Type ℓ} → (B : A → Type ℓ') → List A → Type ℓ' 
ForAllMember B = foldr (_×_ ∘ B) Unit* 


ExistMemberAs : ∀ {ℓ ℓ'} → {A : Type ℓ} → (B : A → Type ℓ') → List A → Type ℓ' 
ExistMemberAs B [] = Lift Empty
ExistMemberAs B (x ∷ x₁) =
  (B x)
    ⊎
  ((IsEmpty (B x)) × ExistMemberAs B x₁)

ExistMemberAs→ : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B B' : A → Type ℓ'} → {l : List A}
                       → (∀ a → B a → B' a) → (∀ a → Dec (B' a))
                       → ExistMemberAs B l → ExistMemberAs B' l 
ExistMemberAs→ {l = x₃ ∷ l} x x₁ (inl x₂) = inl (x _ x₂)
ExistMemberAs→ {l = x₃ ∷ l} x x₁ (inr x₂) =
 dec-rec' _
    inl
    (inr ∘ (_, ExistMemberAs→ x x₁ (proj₂ x₂)))
    (x₁ x₃)

-- ExistMemberAs-map : ∀ {ℓ ℓ'} → {A : Type ℓ} → (B B' : A → Type ℓ')
--                       → (∀ a → B a → B' a) 
--                       → (∀ a → (IsEmpty (B a)) → B' a)
--                       → (l : List A) → ExistMemberAs B l → ExistMemberAs B' l  
-- ExistMemberAs-map = {!!}


ExistMemberAs-¬head→tail : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B : A → Type ℓ'} → {l : List A} → {x : A}
                           → ExistMemberAs B (x ∷ l) → IsEmpty (B x) → ExistMemberAs B l 
ExistMemberAs-¬head→tail (inl x₁) x₂ = empty-rec (x₂ x₁)
ExistMemberAs-¬head→tail (inr x₁) x₂ = proj₂ x₁ 

Is-Prop-ExistMemberAs : ∀ {ℓ ℓ'} → {A : Type ℓ} → (B : A → Type ℓ') → (l : List A) → (∀ x → isProp (B x)) → isProp (ExistMemberAs B l) 
Is-Prop-ExistMemberAs B [] _ = isProp⊥*
Is-Prop-ExistMemberAs B (x₁ ∷ l) x (inl x₂) (inl x₃) = cong inl (x  _ _ _) 
Is-Prop-ExistMemberAs B (x₁ ∷ l) x (inl x₂) (inr x₃) = empty-rec (proj₁ x₃ x₂)
Is-Prop-ExistMemberAs B (x₁ ∷ l) x (inr x₂) (inl x₃) = empty-rec (proj₁ x₂ x₃)
Is-Prop-ExistMemberAs B (x₁ ∷ l) x (inr x₂) (inr x₃) = 
  cong inr (×≡ (isProp¬ _ _ _) (Is-Prop-ExistMemberAs B l x _ _) )
  

instance
  Dec-Pred-ExistMemberAs : ∀ {ℓ ℓ'} {A : Type ℓ} {B : A → Type ℓ'} {{Dec-Pred-B : Dec-Pred B}}
                                        → Dec-Pred (ExistMemberAs B)
  Dec-Pred-ExistMemberAs = record { decide = h }
     where      
       h : (l : List _) → Dec (ExistMemberAs _ l)
       h [] = no lower
       h (x ∷ xs) = Pred-app ⊎? ×-Dec {{Dec-IsEmpty {{Pred-app}}}} {{h xs}}
         

-- this is better encoded like that, than with general rule about turning decidable predicated into propositions,  genreal rule generated too much
-- unresolved instances resolutions
instance
  Dec-ExistMemberAs : ∀ {ℓ ℓ'} {A : Type ℓ} {B : A → Type ℓ'} {{Dec-Pred-B : Dec-Pred B}}
                                        → {l : List A} → Dec (ExistMemberAs B l)
  Dec-ExistMemberAs {ℓ} {ℓ'} {A} {B} ⦃ Dec-Pred-B ⦄ {l} = Pred-app 


FirstIs∙ : ∀ {ℓ ℓ'} → {A : Type ℓ} → (B : A → Type ℓ') → List A → Type ℓ' 
FirstIs∙ B [] = Lift Empty
FirstIs∙ B (x ∷ _) = B x

record FirstIs {ℓ ℓ'} {A : Type ℓ} (B : A → Type ℓ') (l : List A) : Type ℓ' where
  constructor firstIs
  field
    proof : FirstIs∙ B l


instance
  Dec-Pred-FirstIs : ∀ {ℓ ℓ'} → {A : Type ℓ} {B : A → Type ℓ'} 
                                        {{Dec-Pred-B : Dec-Pred B}}
                                        → Dec-Pred (FirstIs B)
  Dec-Pred-FirstIs {{Dec-Pred-B}} = record { decide = h}
     where      
       h : (l : List _) → Dec (FirstIs _ l)
       h [] = no (lower ∘ FirstIs.proof)
       h (x ∷ _) = mapDec firstIs (_∘ FirstIs.proof) (Pred-app)

instance
  Dec-FirstIs : ∀ {ℓ ℓ'} → {A : Type ℓ} {B : A → Type ℓ'} 
                                        {{Dec-Pred-B : Dec-Pred B}} {l : List A}
                                        → Dec (FirstIs B l)
  Dec-FirstIs  ⦃ Dec-Pred-B ⦄ {l} = Pred-app' l 


IsMemberOf : ∀ {ℓ} → {A : Type ℓ} → A → List A → Type ℓ
IsMemberOf a l = ExistMemberAs (a ≡_) l 

ExistFirstBy_WitchIsAlso : ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → (B : A → Type ℓ') → (B' : A → Type ℓ'')  → List A → Type (ℓ-max ℓ' ℓ'') 
ExistFirstBy_WitchIsAlso B B' [] = Lift Empty
ExistFirstBy_WitchIsAlso B B' (x ∷ xs) = (B x × B' x) ⊎ ((IsEmpty (B x) × ExistFirstBy_WitchIsAlso B B' xs))

ExistFirstBy-WitchIsAlso-isProp : ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B : A → Type ℓ'} → {B' : A → Type ℓ''} → (l : List A)
                                    → (∀ x → isProp (B x)) → (∀ x → isProp (B' x)) → isProp (ExistFirstBy_WitchIsAlso B B' l)
ExistFirstBy-WitchIsAlso-isProp (x₁ ∷ l) propB propB' (inl x) (inl x₂) = cong inl (×≡ (propB _ _ _) (propB' _ _ _))
ExistFirstBy-WitchIsAlso-isProp (x₁ ∷ l) _ _ (inl x) (inr x₂) = empty-rec (proj₁ x₂ (proj₁ x))
ExistFirstBy-WitchIsAlso-isProp (x₁ ∷ l) _ _ (inr x) (inl x₂) = empty-rec (proj₁ x (proj₁ x₂))
ExistFirstBy-WitchIsAlso-isProp (x₁ ∷ l) propB propB' (inr x) (inr x₂) =
  cong inr (×≡ (isProp¬ _ _ _) (ExistFirstBy-WitchIsAlso-isProp l propB propB' (proj₂ x) (proj₂ x₂) ))

instance
  Dec-Pred-ExistFirstBy_WitchIsAlso : ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} {B : A → Type ℓ'} {B' : A → Type ℓ''}
                                        {{Dec-Pred-B : Dec-Pred B}} {{Dec-Pred-B' : Dec-Pred B'}}
                                        → Dec-Pred (ExistFirstBy B WitchIsAlso B')
  Dec-Pred-ExistFirstBy_WitchIsAlso = record { decide = h}
     where      
       h : (l : List _) → Dec (ExistFirstBy _ WitchIsAlso _ l)
       h [] = Dec-Empty
       h (x ∷ l) = ×-Dec {{Pred-app}} {{Pred-app}}  ⊎? ×-Dec {{Dec-IsEmpty {{Pred-app}}}} {{h l}}

instance
  Dec-ExistFirstBy_WitchIsAlso : ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} {B : A → Type ℓ'} {B' : A → Type ℓ''}
                                        {{Dec-Pred-B : Dec-Pred B}} {{Dec-Pred-B' : Dec-Pred B'}} {l : List A}
                                        → Dec (ExistFirstBy B WitchIsAlso B' l)
  Dec-ExistFirstBy_WitchIsAlso  ⦃ Dec-Pred-B ⦄ {l} = Pred-app 



ExistFirstByWitchIsAlso→ExistMemberAs :
     ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B : A → Type ℓ'} → {B' : A → Type ℓ''} →
       (l : List A) → ExistFirstBy B WitchIsAlso B' l 
                    → ExistMemberAs B l
ExistFirstByWitchIsAlso→ExistMemberAs (x₂ ∷ l) = 
   map-sum proj₁ (map-prod (idfun _) (ExistFirstByWitchIsAlso→ExistMemberAs l))                    



ExistFirstBy-WitchIsAlso-preppend-lemma : ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B : A → Type ℓ'} → {B' : A → Type ℓ''} →
                                                 (l : List A) → (l' : List A) →
                                                  ExistFirstBy B WitchIsAlso B' l →
                                                 (ExistFirstBy B WitchIsAlso B' (l ++ l'))
ExistFirstBy-WitchIsAlso-preppend-lemma (x₁ ∷ l) l' (inl x) = inl x
ExistFirstBy-WitchIsAlso-preppend-lemma (x₁ ∷ l) l' (inr x) =
  inr ((proj₁ x) , (ExistFirstBy-WitchIsAlso-preppend-lemma l l' (proj₂ x)))



map-ExistingFirstBy_WitchIsAlso : ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → (B : A → Type ℓ') → (B' : A → Type ℓ'')
                                          → (l : List A)  → ExistFirstBy B WitchIsAlso B' l → (∀ x → B x → B' x → A) → List A
map-ExistingFirstBy B WitchIsAlso B' (x₂ ∷ l) (inl x₁) f = f x₂ (proj₁ x₁) (proj₂ x₁) ∷ l
map-ExistingFirstBy B WitchIsAlso B' (x₂ ∷ l) (inr x₁) f = x₂ ∷ map-ExistingFirstBy B WitchIsAlso B' l (proj₂ x₁) f


UniqueBy : ∀ {ℓ ℓ'} → {A : Type ℓ} → (A → A → Type ℓ') → List A → Type ℓ' 
UniqueBy f [] = Lift Unit
UniqueBy f (x ∷ xs) = (IsEmpty (ExistMemberAs (f x) xs)) × UniqueBy f xs

UniqueByDec≡  : ∀ {ℓ ℓ'} → {A : Type ℓ} → {A' : Type ℓ'} → (f : A → A') → {{IsDiscrete A'}} → ∀ l → Dec (UniqueBy {A = A} (λ x x₁ → f x ≡ f x₁) l) 
UniqueByDec≡ _ [] = yes _
UniqueByDec≡ _ (x ∷ xs) = ×-Dec {{Dec-B = UniqueByDec≡ _ xs }}

isProp-UniqueBy : ∀ {ℓ ℓ'} → {A : Type ℓ} → (f : A → A → Type ℓ') → (l : List A) → isProp (UniqueBy f l)
isProp-UniqueBy f [] x y = refl
isProp-UniqueBy f (x₁ ∷ l) _ _ = ×≡ (isProp¬ _ _ _) (isProp-UniqueBy f l _ _)

FilterOut : ∀ {ℓ ℓ'} → {A : Type ℓ} (B : A → Type ℓ') {{Dec-Pred-B : Dec-Pred B}}
                → List A → Σ (List A) λ l → IsEmpty (ExistMemberAs B l)
FilterOut _ ⦃ Dec-Pred-B = Dec-Pred-B ⦄ [] = [] , lower
FilterOut B ⦃ Dec-Pred-B = Dec-Pred-B ⦄ (x ∷ x₁) =
   let q = FilterOut B x₁
   in dec-rec' _ 
        (λ _ → q)
         (λ y → x ∷ fst q , sum-elim y (snd q ∘ proj₂))
         (Dec-Pred.decide Dec-Pred-B x)
-- TODD : decision procedure


ExistFirstBy-WitchIsAlso-FilterOut-lemma : ∀ {ℓ ℓ' ℓ'' ℓ*} → {A : Type ℓ} → {B : A → Type ℓ'} → {B' : A → Type ℓ''} →
                                             {B* : A → Type ℓ*} {{Dec-Pred-B : Dec-Pred B*}} → 
                                                 (l : List A) → (∀ a → B a → IsEmpty (B* a)) →
                                                  ExistFirstBy B WitchIsAlso B' l →
                                                 (ExistFirstBy B WitchIsAlso B' (fst (FilterOut B* l)))
ExistFirstBy-WitchIsAlso-FilterOut-lemma ⦃ Dec-Pred-B = Dec-Pred-B ⦄ (x₂ ∷ l) f  x with x | Dec-Pred.decide Dec-Pred-B x₂
... | inl x₁ | yes p = empty-rec (f _ (proj₁ x₁) p)
... | inr x₁ | yes p = ExistFirstBy-WitchIsAlso-FilterOut-lemma _ f (proj₂ x₁)
... | inl x₁ | no ¬p = inl x₁
... | inr x₁ | no ¬p = map-sum (idfun _) (map-prod (idfun _) (ExistFirstBy-WitchIsAlso-FilterOut-lemma _ f)) x

ExistFirstBy-WitchIsAlso-FilterOut-lemma2' : ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B : A → Type ℓ'} → {B' : A → Type ℓ''} →
                                              {{Dec-Pred-B : Dec-Pred B}}  → 
                                                 (l : List A) → (f : (x : A) → B x → B' x → A)
                                                 → IsEmpty (ExistFirstBy B WitchIsAlso B' (fst (FilterOut B l)))
ExistFirstBy-WitchIsAlso-FilterOut-lemma2' {B = B} {B' = B'} {{Dec-Pred-B}} (x₂ ∷ l) f  = 
  dec-elim
    (λ q → ExistFirstBy B WitchIsAlso B'
      (fst
       (dec-rec' (B x₂) (λ _ → FilterOut B l)
        (λ y → x₂ ∷ fst (FilterOut B l) , sum-elim (y) (snd (FilterOut B l) ∘ proj₂))
        (q))) →
      Empty)
    (λ _ → ExistFirstBy-WitchIsAlso-FilterOut-lemma2' l f)
    (λ x → sum-elim (λ a → x (proj₁ a))
     λ b → ExistFirstBy-WitchIsAlso-FilterOut-lemma2' l f (proj₂ b))
    (Dec-Pred.decide Dec-Pred-B x₂)



ExistFirstByWitchIsAlso-Match : ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B B* : A → Type ℓ'} → {B' : A → Type ℓ''}
                                                                                   → {B'* : A → Type ℓ''} →
                                                 {l : List A} → 
                                                  ExistFirstBy B WitchIsAlso B' l →
                                                  ExistFirstBy B* WitchIsAlso B'* l → DecPropΣ
ExistFirstByWitchIsAlso-Match {l = x₂ ∷ l} (inl x) (inl x₁) = Unit-dp
ExistFirstByWitchIsAlso-Match {l = x₂ ∷ l} (inl x) (inr x₁) = Empty-dp
ExistFirstByWitchIsAlso-Match {l = x₂ ∷ l} (inr x) (inl x₁) = Empty-dp
ExistFirstByWitchIsAlso-Match {l = x₂ ∷ l} (inr x) (inr x₁) = ExistFirstByWitchIsAlso-Match (proj₂ x) (proj₂ x₁)

-- ExistMemberAs-map-subst : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B B' : A → Type ℓ'}
--                      → (l : List A)
--                      → (∀ {a} → B a → B' a) → (∀ {a} → B' a → B a)
--                      → ExistMemberAs B l → ExistMemberAs B' l 
-- ExistMemberAs-map-subst (x₃ ∷ l) x x₁ = map-sum x (map-prod (_∘ x₁) (ExistMemberAs-map-subst  l x x₁))


-- ExistMemberAs-mapExisting : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B : A → Type ℓ'}
--                     → (A → A) → (l : List A) 
--                     → ExistMemberAs B l
--                     → List A
-- ExistMemberAs-mapExisting f (x₁ ∷ l) (inl x) = f x₁ ∷ l
-- ExistMemberAs-mapExisting f (x₁ ∷ l) (inr x) = x₁ ∷ (ExistMemberAs-mapExisting f l (proj₂ x))

-- ExistMemberAs-mapExisting-stillUnique-lem :
--                     ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B : A → Type ℓ'} → {R : A → A → Type ℓ''}
--                     → (f : A → A) → (l : List A) → (y : ExistMemberAs B l)
--                     → (∀ {a a'} →  R a (f a') → R a a' )
--                     → (∀ {a a'} → R a a' → R a (f a') )
--                     → ∀ x₃
--                     → ExistMemberAs (R x₃) (ExistMemberAs-mapExisting f l y)
--                     → ExistMemberAs (R x₃) l
-- ExistMemberAs-mapExisting-stillUnique-lem f (x₄ ∷ l) (inl x₅) x x₁ x₃ = map-sum x (map-prod (_∘ x₁) (idfun _))
-- ExistMemberAs-mapExisting-stillUnique-lem f (x₄ ∷ l) (inr x₅) x x₁ x₃ =
--   map-sum (idfun _) (map-prod (idfun _)
--       (ExistMemberAs-mapExisting-stillUnique-lem f l (proj₂ x₅)
--         (λ {a} {a'} x₅ → x {a} {a'} x₅)
--             (λ {a} {a'} x₅ → x₁ {a} {a'} x₅)
--         x₃))

-- ExistMemberAs-mapExisting-stillUnique :
--                     ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B : A → Type ℓ'} → {R : A → A → Type ℓ''}
--                     → (f : A → A) → (l : List A) → (y : ExistMemberAs B l)
--                     → (∀ {a a'} →  R (f a) a' → R a a' )
--                     → (∀ {a a'} → R a a' → R (f a) a' )
--                     → (∀ {a a'} →  R a (f a') → R a a' )
--                     → (∀ {a a'} → R a a' → R a (f a') )
--                     → UniqueBy R l
--                     → UniqueBy R (ExistMemberAs-mapExisting f l y)
-- ExistMemberAs-mapExisting-stillUnique f (x₃ ∷ l) (inl x₄) x x₁ z z₁ x₂ =
--   proj₁ x₂ ∘ ExistMemberAs-map-subst _ (x) (x₁) , (proj₂ x₂)
-- ExistMemberAs-mapExisting-stillUnique f (x₃ ∷ l) (inr x₄) x x₁ z z₁ x₂ =
--   proj₁ x₂ ∘ (λ a → ExistMemberAs-mapExisting-stillUnique-lem f l (proj₂ x₄)
--             (λ {a} {a'} x₅ → z {a} {a'} x₅)
--             (λ {a} {a'} x₅ → z₁ {a} {a'} x₅) x₃ a)
--          , ExistMemberAs-mapExisting-stillUnique _ _ _ x x₁ z z₁ (proj₂ x₂)


-- ExistMemberAs-mapExisting-mapProp' :
--                     ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B B' : A → Type ℓ'} → {R : A → A → Type ℓ''}
--                       (l : List A) 
--                     → UniqueBy R l
--                     → (a* : A)
--                     → (∀ (a a' : A) → B' a' → B a  → R a' a)
--                     → (∀ (a a' : A) → B' a' → R a' a → IsEmpty (B a) → B' a)
--                     → B' a* → ExistMemberAs B l → ExistMemberAs (R a*) l
-- ExistMemberAs-mapExisting-mapProp' (x₃ ∷ l) x a* x₁ x₁' x₂ =
--   map-sum (x₁ _ _ x₂)
--     λ x₄ →
--           (λ x₅ → proj₁ x (ExistMemberAs-mapExisting-mapProp' l (proj₂ x) x₃ x₁ x₁'
--             ((x₁' _ _ x₂ x₅ (proj₁ x₄))) (proj₂ x₄)))
--          , (ExistMemberAs-mapExisting-mapProp' l (proj₂ x) a* x₁ x₁' x₂ (proj₂ x₄))
         

-- ExistMemberAs-mapExisting-mapProp :
--                     ∀ {ℓ ℓ' ℓ'''} → {A : Type ℓ} → {B B' : A → Type ℓ'} → {R : A → A → Type ℓ'''}
--                     → (f : A → A) → (l : List A) 
--                     → (∀ {a} → B a → B' (f a)) 
--                     → UniqueBy R l
--                     → (∀ (a a' : A) →  B' a' → B a → R a' a)
--                     → (∀ (a a' : A) → B' a' → R a' a → IsEmpty (B a) → B' a)
--                     → (y : ExistMemberAs B l)
--                     → ExistMemberAs B' (ExistMemberAs-mapExisting f l y)
-- ExistMemberAs-mapExisting-mapProp f (x₂ ∷ l) x _ _ _ (inl x₃) = inl (x x₃)
-- ExistMemberAs-mapExisting-mapProp f (x₂ ∷ l) x u q q' (inr x₃) =
--   inr ((λ x₄ → proj₁ u (ExistMemberAs-mapExisting-mapProp' l (proj₂ u) x₂ q q' x₄ (proj₂ x₃)))
--      , ExistMemberAs-mapExisting-mapProp f l x (proj₂ u) q q' (proj₂ x₃))


-- ExistMemberAs-mapExisting-help : ∀ {ℓ ℓ'} → {A C : Type ℓ} → {B B' : A → Type ℓ'} → {r : A → C} → {{Discrete C}}
--                                  → ∀ l
--                                  → (UniqueBy {A = A} (λ x x₁ → r x ≡ r x₁) l)
--                                  → ExistMemberAs B l
--                                  → (f : A → Σ _ B')
--                                  → (∀ a → (r ∘ (fst ∘ f)) a ≡ r a)
--                                  → ((a a' : A) → B' a' → B a → r a' ≡ r a)
--                                  → ((a a' : A) → B' a' → r a' ≡ r a → IsEmpty (B a) → B' a)
--                                  → Σ _ λ l' → ExistMemberAs B' l' × (UniqueBy {A = A} (λ x x₁ → r x ≡ r x₁) l')
-- ExistMemberAs-mapExisting-help l u e f p l1 l2 =
--    ExistMemberAs-mapExisting (fst ∘ f) l e  ,
--       (ExistMemberAs-mapExisting-mapProp (fst ∘ f) l (λ {a} _ → snd (f a)) u l1 l2 e
--          , ExistMemberAs-mapExisting-stillUnique (fst ∘ f) l e
--             (λ x → sym (p _)  ∙ x)
--             (λ x → p _ ∙ x)
--             (λ x → x ∙ p _)
--             (λ x → x ∙ sym (p _))
--             u
--          )


ExistMemberAs-map-subst : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B B' : A → Type ℓ'}
                     → (l : List A)
                     → (∀ {a} → B a → B' a) → (∀ {a} → B' a → B a)
                     → ExistMemberAs B l → ExistMemberAs B' l 
ExistMemberAs-map-subst (x₃ ∷ l) x x₁ = map-sum x (map-prod (_∘ x₁) (ExistMemberAs-map-subst  l x x₁))


ExistMemberAs-mapExisting : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B : A → Type ℓ'}
                    → (∀ x → B x → A) → (l : List A) 
                    → ExistMemberAs B l
                    → List A
ExistMemberAs-mapExisting f (x₁ ∷ l) (inl x) = f x₁ x ∷ l
ExistMemberAs-mapExisting f (x₁ ∷ l) (inr x) = x₁ ∷ (ExistMemberAs-mapExisting f l (proj₂ x))

ExistMemberAs-mapExisting-stillUnique-lem :
                    ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B : A → Type ℓ'} → {R : A → A → Type ℓ''}
                    → (f : (x : A) → B x → A) → (l : List A) → (y : ExistMemberAs B l)
                    → (∀ {a a'} → ∀ q → R a (f a' q) → R a a' )
                    → (∀ {a a'} → ∀ q → R a a' → R a (f a' q) )
                    → ∀ x₃
                    → ExistMemberAs (R x₃) (ExistMemberAs-mapExisting f l y)
                    → ExistMemberAs (R x₃) l
ExistMemberAs-mapExisting-stillUnique-lem f (x₄ ∷ l) (inl x₅) x x₁ x₃ =
  map-sum (x _) (map-prod (_∘ x₁ _) (idfun _))
ExistMemberAs-mapExisting-stillUnique-lem f (x₄ ∷ l) (inr x₅) x x₁ x₃ = 
  map-sum (idfun _) (map-prod (idfun _)
      (ExistMemberAs-mapExisting-stillUnique-lem f l (proj₂ x₅)
        (λ {a} {a'} x₅ → x {a} {a'} x₅)
            (λ {a} {a'} x₅ → x₁ {a} {a'} x₅)
        x₃))

ExistMemberAs-mapExisting-stillUnique :
                    ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B : A → Type ℓ'} → {R : A → A → Type ℓ''}
                    → (f : (x : A) → B x → A) → (l : List A) → (y : ExistMemberAs B l)
                    → (∀ {a a'} → ∀ q → R (f a q) a' → R a a' )
                    → (∀ {a a'} → ∀ q → R a a' → R (f a q) a' )
                    → (∀ {a a'} → ∀ q → R a (f a' q) → R a a' )
                    → (∀ {a a'} → ∀ q → R a a' → R a (f a' q) )
                    → UniqueBy R l
                    → UniqueBy R (ExistMemberAs-mapExisting f l y)

ExistMemberAs-mapExisting-stillUnique f (x₃ ∷ l) (inl x₄) x x₁ z z₁ x₂ =
  proj₁ x₂ ∘ ExistMemberAs-map-subst _ (x _) (x₁ _) , (proj₂ x₂)
ExistMemberAs-mapExisting-stillUnique f (x₃ ∷ l) (inr x₄) x x₁ z z₁ x₂ =
  proj₁ x₂ ∘ (λ a → ExistMemberAs-mapExisting-stillUnique-lem f l (proj₂ x₄)
            (λ {a} {a'} x₅ → z {a} {a'} x₅)
            (λ {a} {a'} x₅ → z₁ {a} {a'} x₅) x₃ a)
         , ExistMemberAs-mapExisting-stillUnique _ _ _ x x₁ z z₁ (proj₂ x₂)


ExistMemberAs-mapExisting-mapProp' :
                    ∀ {ℓ ℓ' ℓ''} → {A : Type ℓ} → {B B' : A → Type ℓ'} → {R : A → A → Type ℓ''}
                      (l : List A) 
                    → UniqueBy R l
                    → (a* : A)
                    → (∀ (a a' : A) → B' a' → B a  → R a' a)
                    → (∀ (a a' : A) → B' a' → R a' a → IsEmpty (B a) → B' a)
                    → B' a* → ExistMemberAs B l → ExistMemberAs (R a*) l
ExistMemberAs-mapExisting-mapProp' (x₃ ∷ l) x a* x₁ x₁' x₂ = 
  map-sum (x₁ _ _ x₂)
    λ x₄ →
          (λ x₅ → proj₁ x (ExistMemberAs-mapExisting-mapProp' l (proj₂ x) x₃ x₁ x₁'
            ((x₁' _ _ x₂ x₅ (proj₁ x₄))) (proj₂ x₄)))
         , (ExistMemberAs-mapExisting-mapProp' l (proj₂ x) a* x₁ x₁' x₂ (proj₂ x₄))
         

ExistMemberAs-mapExisting-mapProp :
                    ∀ {ℓ ℓ' ℓ'''} → {A : Type ℓ} → {B B' : A → Type ℓ'} → {R : A → A → Type ℓ'''}
                    → (f : (x : A) → B x → A) → (l : List A) 
                    → (∀ {a} → ∀ q → B' (f a q)) 
                    → UniqueBy R l
                    → (∀ (a a' : A) →  B' a' → B a → R a' a)
                    → (∀ (a a' : A) → B' a' → R a' a → IsEmpty (B a) → B' a)
                    → (y : ExistMemberAs B l)
                    → ExistMemberAs B' (ExistMemberAs-mapExisting f l y)
ExistMemberAs-mapExisting-mapProp f (x₂ ∷ l) x _ _ _ (inl x₃) = inl (x x₃)
ExistMemberAs-mapExisting-mapProp f (x₂ ∷ l) x u q q' (inr x₃) =
  inr ((λ x₄ → proj₁ u (ExistMemberAs-mapExisting-mapProp' l (proj₂ u) x₂ q q' x₄ (proj₂ x₃)))
     , ExistMemberAs-mapExisting-mapProp f l x (proj₂ u) q q' (proj₂ x₃))


ExistMemberAs-mapExisting-help : ∀ {ℓ ℓ'} → {A C : Type ℓ} → {B B' : A → Type ℓ'} → {r : A → C} → {{Discrete C}}
                                 → ∀ l
                                 → (UniqueBy {A = A} (λ x x₁ → r x ≡ r x₁) l)
                                 → ExistMemberAs B l
                                 → (f : ∀ a → B a → Σ _ B')
                                 → (∀ a → ∀ q → (r (fst (f a q)) ≡ r a))
                                 → ((a a' : A) → B' a' → B a → r a' ≡ r a)
                                 → ((a a' : A) → B' a' → r a' ≡ r a → IsEmpty (B a) → B' a)
                                 → Σ _ λ l' → ExistMemberAs B' l' × (UniqueBy {A = A} (λ x x₁ → r x ≡ r x₁) l')
ExistMemberAs-mapExisting-help l u e f p l1 l2 =
 let f' = λ x x₁ → fst (f x x₁) 
 in ExistMemberAs-mapExisting (f') l e  ,
      (ExistMemberAs-mapExisting-mapProp (f') l (λ {a} q → snd (f a q)) u l1 l2 e
         , ExistMemberAs-mapExisting-stillUnique (f') l e
            (λ x →  sym (p _ _) ∙_)
            (λ x → p _ _ ∙_)
            (λ x → _∙ p _ _)
            (λ x → _∙ sym (p _ _))
            u
         )


-- module moveFrwrdChangedOrRemove
--             {ℓ} (A : Type ℓ) (B : A → DecPropΣ)
--               (makeNotB : (a : A) → ⟨ B a ⟩ → Σ _ (IsEmpty ∘ fst ∘ B)  )where

--   -- MoveFrwrdChangedOrRemove : List A → Type₀
--   -- MoveFrwrdChangedOrRemove [] = Unit
--   -- MoveFrwrdChangedOrRemove (x ∷ xs) =
--   --    (⟨ B x ⟩ × Maybe ℕ ) ⊎ IsEmpty ⟨ B x ⟩
--   --   -- dec-rec ⟨ B x ⟩ {{proj₁ (snd (B x))}}
--   --   --   (λ _ → Maybe ℕ)
--   --   --   (λ _ → MoveFrwrdChangedOrRemove xs)


-- injAtMany : ∀ {ℓ ℓ'} {A : Type ℓ} {B : A → Type ℓ'} → List (Maybe (Σ A B)) → List A →  List A
-- injAtMany [] x₁ = x₁
-- injAtMany (x ∷ x₂) [] = []
-- injAtMany (nothing ∷ x₂) (x₁ ∷ x₃) = x₁ ∷ injAtMany x₂ x₃
-- injAtMany (just x ∷ x₂) l@(_ ∷ _) = fst x ∷ injAtMany x₂ l


-- pick : ∀ {ℓ} {A : Type ℓ} → ℕ → A → List A → (A × List A)
-- pick zero x xs = x , xs
-- pick (suc n) x [] = x , []
-- pick (suc n) x (x₁ ∷ xs) =
--   map-prod (idfun _) (x ∷_) (pick n x₁ xs)

-- bringToFront : ∀ {ℓ} {A : Type ℓ} → ℕ → ℕ → List A → List A
-- bringToFront x x₁ [] = []
-- bringToFront zero k (x ∷ xs) =
--   let z =  (pick k x xs)
--    in proj₁ z ∷ proj₂ z
-- bringToFront (suc n) k (x ∷ xs) =
--      x ∷ bringToFront n k xs


BTF' : ∀ {ℓ} {A : Type ℓ} → List A → Type₀
BTF' [] = Empty
BTF' (_ ∷ xs) = Maybe (BTF' xs)


BTF : ∀ {ℓ} {A : Type ℓ} → List A → Type₀
BTF [] = Empty
BTF (x ∷ xs) = BTF xs ⊎ BTF' (xs) 


pick :  ∀ {ℓ} {A : Type ℓ} → ∀ l → BTF' l → A
pick (a ∷ l) nothing = a
pick (_ ∷ l) (just x) = pick l x

updAt :  ∀ {ℓ} {A : Type ℓ} → A → ∀ l → BTF' l → List A
updAt a (x ∷ l) y =
  caseMaybe a x y
  ∷
  recMaybe l (updAt a l) y
-- updAt a (_ ∷ l) nothing = a ∷ l
-- updAt a (x ∷ l) (just y) = x ∷ updAt a l y


btf :  ∀ {ℓ} {A : Type ℓ} → (l : List A) → BTF l → List A
btf [] _ = []
btf (x ∷ xs) y =
   sum-rec (const x) (pick xs) y
   ∷
   sum-rec (btf xs) (updAt x xs) y


haveSameL : ∀ {ℓ} {A : Type ℓ} → List A → List A → Type₀
haveSameL [] [] = Unit
haveSameL [] (x ∷ x₁) = Empty
haveSameL (x ∷ x₁) [] = Empty
haveSameL (x ∷ x₁) (x₂ ∷ x₃) = haveSameL x₁ x₃

haveSameL-refl : ∀ {ℓ} {A : Type ℓ} → (l : List A) → haveSameL l l
haveSameL-refl [] = tt
haveSameL-refl (_ ∷ l) = haveSameL-refl l

BTF-trans' : ∀ {ℓ} {A : Type ℓ} → {l l' : List A} → haveSameL l l' → BTF' l → BTF' l'
BTF-trans' {l = _ ∷ _} {_ ∷ _} x  = map-Maybe (BTF-trans' x)

BTF-trans : ∀ {ℓ} {A : Type ℓ} → {l l' : List A} → haveSameL l l' → BTF l → BTF l'
BTF-trans {l = _ ∷ _} {_ ∷ _} y = map-sum (BTF-trans y) (BTF-trans' y) 


updAt-sameL : ∀ {ℓ} {A : Type ℓ} → ∀ a → (l : List A) → (bb : BTF' l) → haveSameL l (updAt a l bb)
updAt-sameL _ (x ∷ l) nothing = haveSameL-refl l
updAt-sameL _ (x ∷ l) (just x₁) = updAt-sameL _ _ x₁

btf-sameL : ∀ {ℓ} {A : Type ℓ} → (l : List A) → (bb : BTF l) → haveSameL l (btf l bb)
btf-sameL (_ ∷ _) (inl x₁) = btf-sameL _ x₁
btf-sameL (_ ∷ _) (inr x₁) = updAt-sameL _ _ x₁

{-# TERMINATING #-}
btf-more : ∀ {ℓ} {A : Type ℓ} → (l : List A) → List (BTF l) → List A
btf-more l [] = l
btf-more l (x ∷ x₁) = btf-more (btf l x) (map-List (BTF-trans (btf-sameL l x)) x₁)

-- BTF'-step' : ∀ {ℓ} {A : Type ℓ} → ∀ a → (l : List A) → (bb : BTF' l) → BTF' l → BTF' (updAt a l bb)
-- BTF'-step' _ (_ ∷ _) =
--   map-Maybe ∘ 
--    (maybe-elim {B = λ bb → BTF' _ → BTF' (recMaybe _ (updAt _ _) bb)}
--       (idfun _)
--       (BTF'-step' _ _))

-- BTF'-step : ∀ {ℓ} {A : Type ℓ} → (l : List A) → (bb : BTF l) → BTF' l → BTF' (btf l bb)
-- BTF'-step (_ ∷ _) =
--    map-Maybe ∘
--      ((sum-elim {C = λ y → BTF' _ → BTF' (sum-rec (btf _) (updAt _ _) y)}
--        (BTF'-step _) (BTF'-step' _ _)))

-- BTF-step : ∀ {ℓ} {A : Type ℓ} → (l : List A) → (bb : BTF l) → BTF l → BTF (btf l bb)
-- BTF-step (x ∷ l) y =
--   map-sum
--     (sum-elim {C = λ y → BTF l → BTF (sum-rec (btf l) (updAt x l) y)}
--       (BTF-step _)
--       {!!}
--        y)
--     (sum-elim {C = λ y → BTF' l → BTF' (sum-rec (btf l) (updAt x l) y)}
--       {!!}
--       (BTF'-step' _ _)
--       y)





haveSameL-filter-lemma : ∀ {ℓ ℓ'} {A : Type ℓ} {B : Type ℓ'} → (f : A → Maybe B) → 
                             (l l' : List A) →
                             haveSameL l l' × ForAllMember (λ x → ⟨ maybe-eqCase (f (proj₁ x)) (f (proj₂ x)) ⟩) (zip l l' )
                             → haveSameL (filterMap f l) (filterMap f l')
haveSameL-filter-lemma f [] [] (x , x₁) = tt
haveSameL-filter-lemma f (x₂ ∷ l) (x₃ ∷ l') (x , x₁) with (f x₂) | (f x₃) | proj₁ x₁
... | nothing | nothing | _ = haveSameL-filter-lemma f l l' (x , (proj₂ x₁))
... | just x₄ | just x₅ | _ = haveSameL-filter-lemma f l l' (x  , (proj₂ x₁))



mb-Pred : ∀ {ℓ ℓ'} → {A : Type ℓ} → (B : A → Type ℓ') → Maybe A → Type ℓ' 
mb-Pred B = recMaybe Unit* B

existFWIA-filter : ∀ {ℓ ℓ'} {A A' : Type ℓ} {B B* : A → Type ℓ'}
                                 {B' B'* : A' → Type ℓ'} → {f : A → Maybe A'}
                       → (∀ a → recMaybe (B a × B* a → Empty) (λ a' → (((B a × B* a) → (B' a' × B'* a')) × (B' a' → B a))) (f a))
                       → {l : List A}
                       → ExistFirstBy B WitchIsAlso B* l
                       → ExistFirstBy B' WitchIsAlso B'* (filterMap f l)
existFWIA-filter {f = f} x {a ∷ l} x₂ with (f a) | x₂ | x a  
... | nothing | inl x₄ | x' = empty-elim (x' x₄)
... | nothing | inr x₄ | x' = existFWIA-filter {f = f} x (proj₂ x₄) 
... | just x₄ | inl x₅ | x' = inl (proj₁ x' x₅)
... | just x₄ | inr x₅ | x' = inr ( proj₁ x₅ ∘ proj₂ x' , existFWIA-filter {f = f} x (proj₂ x₅))


BTF-Ex : ∀ {ℓ} → {A : Type ℓ} → {B B' : A → Type ℓ} → {l : List A} → (ExistFirstBy B WitchIsAlso B' l) → Type₀
BTF-Ex {l = x₁ ∷ l} (inl x) = Empty
BTF-Ex {l = x₁ ∷ l} (inr x) = Maybe (BTF-Ex (proj₂ x))

pick-Ex : ∀ {ℓ} → {A : Type ℓ} → {B B' : A → Type ℓ} → {l : List A} → (ex : ExistFirstBy B WitchIsAlso B' l) 
                       → Σ A λ x → B x × B' x
pick-Ex {l = x ∷ l} (inl x₁) = x , x₁
pick-Ex {l = x ∷ l} (inr x₁) = pick-Ex (proj₂ x₁)

rem-Ex : ∀ {ℓ} → {A : Type ℓ} → {B B' : A → Type ℓ} → {l : List A} → (ex : ExistFirstBy B WitchIsAlso B' l) 
                       → List A
rem-Ex {l = x ∷ l} (inl x₁) = l
rem-Ex {l = x ∷ l} (inr x₁) = x ∷ rem-Ex (proj₂ x₁)



-- rem-Ex-property : ∀ {ℓ} → {A C : Type ℓ} → {toC : A → C} → {B' : A → Type ℓ} → {l : List A} → {c : C}
--                                                 → (ex : ExistFirstBy (c ≡_) ∘ toC WitchIsAlso B' l) 
--                        → (let l' = rem-Ex in ∀ c → ExistFirstBy (c ≡_) ∘ toC  WitchIsAlso B' l
--                                                     → ExistFirstBy (c ≡_) ∘ toC  WitchIsAlso B' l')

-- rem-Ex-property = ?

rem-Ex-property : ∀ {ℓ} → {A C : Type ℓ} → {toC : A → C} → {B' : A → Type ℓ} → {l : List A} → {c : C}
                                                → (ex : ExistFirstBy (c ≡_) ∘ toC WitchIsAlso B' l) 
                       → (let l' = rem-Ex ex in ∀ c' → IsEmpty (c ≡ c') →
                                                      ExistFirstBy (c' ≡_) ∘ toC  WitchIsAlso B' l
                                                    → IsEmpty (c' ≡ toC (fst (pick-Ex ex))) × ExistFirstBy (c' ≡_) ∘ toC  WitchIsAlso B' l')

rem-Ex-property {l = x₂ ∷ l} (inl x₁) c' x (inl x₃) = empty-elim (x (proj₁ x₁  ∙ sym (proj₁ x₃)))
rem-Ex-property {l = x₂ ∷ l} (inl x₁) c' x (inr x₃) = x₃
rem-Ex-property {l = x₂ ∷ l} (inr x₁) c' x (inl x₃) =
   (λ x₄ → proj₁ x₁ (proj₁ (snd (pick-Ex (proj₂ x₁))) ∙ sym x₄ ∙ proj₁ x₃)) , (inl x₃)
rem-Ex-property {l = x₂ ∷ l} (inr x₁) c' x (inr x₃) =
  let z = rem-Ex-property (proj₂ x₁) c' x (proj₂ x₃)
  in proj₁ z , inr (proj₁ x₃ , (proj₂ z))


pick-Ex-property : ∀ {ℓ} → {A C : Type ℓ} → {toC : A → C} → {B' : A → Type ℓ} → {l : List A} → {c : C}
                                                → (ex : ExistFirstBy (c ≡_) ∘ toC WitchIsAlso B' l) 
                       → (let l' = rem-Ex ex in ∀ c' → (c ≡ c') →
                                                      ExistFirstBy (c' ≡_) ∘ toC  WitchIsAlso B' l
                                                    → (c' ≡ toC (fst (pick-Ex ex))) × B' ((fst (pick-Ex ex))))

pick-Ex-property {l = x₂ ∷ l} (inl x₁) c' x (inl x₃) = x₃
pick-Ex-property {l = x₂ ∷ l} (inl x₁) c' x (inr x₃) = empty-elim (proj₁ x₃ (sym x ∙ proj₁ x₁))
pick-Ex-property {l = x₂ ∷ l} (inr x₁) c' x (inl x₃) = empty-elim (proj₁ x₁ (x ∙ proj₁ x₃))
pick-Ex-property {l = x₂ ∷ l} (inr x₁) c' x (inr x₃) = pick-Ex-property (proj₂ x₁) c' x (proj₂ x₃)

bth-Ex : ∀ {ℓ} → {A C : Type ℓ} → {toC : A → C} → {B' : A → Type ℓ} → {l : List A} → {c : C}
                                                → (ex : ExistFirstBy (c ≡_) ∘ toC WitchIsAlso B' l) 
                       → List A
bth-Ex ex = fst (pick-Ex ex) ∷ rem-Ex ex



bth-Ex-property : ∀ {ℓ} → {A C : Type ℓ} → {toC : A → C} → {B' : A → Type ℓ} → {l : List A} → {c : C}
                                                → (ex : ExistFirstBy (c ≡_) ∘ toC WitchIsAlso B' l) 
                       → (let l' = bth-Ex ex in ∀ c' → Dec (c ≡ c')
                                                          → ExistFirstBy (c' ≡_) ∘ toC  WitchIsAlso B' l
                                                          → ExistFirstBy (c' ≡_) ∘ toC  WitchIsAlso B' l')
bth-Ex-property {l = x₁ ∷ l} (inl x₂) c _ x = x
bth-Ex-property {l = x₁ ∷ []} (inr (x₂ , ())) c _ x
bth-Ex-property {l = x₁ ∷ x ∷ l} (inr x₂) c _ (inl x₃) =
  inr ((λ y → proj₁ x₂ (proj₁ (snd (pick-Ex (proj₂ x₂))) ∙ sym y ∙ proj₁ x₃)) , (inl x₃))
bth-Ex-property {l = x₁ ∷ x ∷ l} (inr x₂) c (yes p) (inr x₃) =
  inl (pick-Ex-property (inr x₂) c p (inr x₃))
bth-Ex-property {l = x₁ ∷ x ∷ l} (inr x₂) c (no ¬p) (inr x₃) =
  inr (rem-Ex-property (inr x₂) c ¬p (inr x₃))

bth-Ex-property-BTF-Ex : ∀ {ℓ} → {A C : Type ℓ} → {toC : A → C} → {B' : A → Type ℓ} → {l : List A} → {c : C}
                                                → (ex : ExistFirstBy (c ≡_) ∘ toC WitchIsAlso B' l) 
                       → (let l' = bth-Ex ex in ∀ c' → (Dec-c≡c' : Dec (c ≡ c')) → ∀ ex'
                                                          → BTF-Ex ex'
                                    → (⟨ ExistFirstByWitchIsAlso-Match ex ex' ⟩) ⊎ BTF-Ex (bth-Ex-property ex c' Dec-c≡c' ex'))
bth-Ex-property-BTF-Ex {l = x ∷ l} (inl x₂) c' Dec-c≡c' ex' x₁ = inr x₁
bth-Ex-property-BTF-Ex {l = x ∷ []} (inr (x₂ , ())) c' Dec-c≡c' ex' x₁
bth-Ex-property-BTF-Ex {l = x ∷ x₃ ∷ l} (inr x₂) c' (yes p) (inr x₄) x₁ = inl {!!}
bth-Ex-property-BTF-Ex {l = x ∷ x₃ ∷ l} (inr x₂) c' (no ¬p) (inr x₄) x₁ = {!!}

btf-Ex : ∀ {ℓ} → {A C : Type ℓ} → {{IsDiscrete C}} → {toC : A → C} → {B' : A → Type ℓ} → {l : List A} → ∀ {c}
                      → (ex : ExistFirstBy ((c ≡_) ∘ toC) WitchIsAlso B' l) 
                       → BTF-Ex ex
                       → Σ (List A)
                           λ l' →
                            ∀ c' →
                            Σ ( ExistFirstBy (c' ≡_) ∘ toC  WitchIsAlso B' l
                                → ExistFirstBy (c' ≡_) ∘ toC  WitchIsAlso B' l')
                                λ f → ∀ ex' → BTF-Ex ex' → (⟨ ExistFirstByWitchIsAlso-Match ex ex' ⟩) ⊎ (BTF-Ex (f ex'))
btf-Ex {l = x₁ ∷ l} {c} (inr x₂) nothing =
  
  bth-Ex (inr x₂)  , λ c' → bth-Ex-property (inr x₂) c' (_ ≟ _)
                                 , bth-Ex-property-BTF-Ex (inr x₂) c' (_ ≟ _) 
btf-Ex {l = x₁ ∷ l} (inr x₂) (just x) =
  let (z , q) = btf-Ex {l = l} (proj₂ x₂) x
  in (x₁ ∷ z) , λ c' → map-sum (idfun _) (map-prod (idfun _) (fst (q c')))
                     , sum-elim (λ _ ()) λ b →  recMaybe (inr nothing) (map-sum (idfun _) just ∘ snd (q c') (proj₂ b))
                      -- ((map-Maybe ∘ (snd (q c') ∘ (proj₂))))



Ex-btfs : ∀ {ℓ} → {A D : Type ℓ} → {B B' : D → A → Type ℓ} → ℕ → (l : List A) → Type ℓ
Ex-btfs zero _ = Unit*
Ex-btfs {D = D} {B} {B'} (suc x) l =
  (Σ D λ d → (Σ (ExistFirstBy (B d) WitchIsAlso (B' d) l) BTF-Ex))
    × Ex-btfs {D = D} {B} {B'} x l


ex-btfs : ∀ {ℓ} → {A C : Type ℓ} → {{IsDiscrete C}} → {toC : A → C} → {B' : A → Type ℓ}
              → ∀ k → (l : List A)
              → Ex-btfs {D = C} {λ x x₁ → x ≡ (toC x₁)} {λ x x₁ → B' x₁} k l → List A

ex-btfs-trans : ∀ {ℓ} → {A C : Type ℓ} → {{IsDiscrete-C : IsDiscrete C}} → {toC : A → C} → {B' : A → Type ℓ}
                     → ∀ k → (l : List A)
                      → (x : Ex-btfs {D = C} {λ x x₁ → x ≡ (toC x₁)} {λ x x₁ → B' x₁} k l)
                      → Ex-btfs {D = C} {λ x x₁ → x ≡ (toC x₁)} {λ x x₁ → B' x₁} k (ex-btfs k l x)

ex-btfs zero l x = l
ex-btfs (suc k) [] (() , x₁)
ex-btfs (suc k) (x₁ ∷ l) (x , x₂) = 
   ex-btfs k {!!} {!!} 

ex-btfs-trans = {!!}

-- ex-btfs zero l x = l
-- ex-btfs (suc k) l ((c , ex , btf-ex) , x₁) =
--   let z = {!btf-Ex !}
--   in ex-btfs k {!!} {!!}
--   -- ex-btfs k (ex-btfs k l x₁) (ex-btfs-trans _ _ x₁)

-- ex-btfs-trans zero l x = x
-- ex-btfs-trans (suc k) [] (() , x₁)
-- ex-btfs-trans (suc k) (x₁ ∷ l) (x , x₂) =
--   {!!}
--   -- ((fst x) , ({!fst (snd x)!} , {!!})) , {!!}


-- btf-Ex-property : ∀ {ℓ} → {A C : Type ℓ} → {{IsDiscrete C}} → {toC : A → C} → {B' : C → A → Type ℓ} → {l : List A}
--                                                 → ∀ k → (x : Ex-btfs {D = C} {λ x x₁ → x ≡ toC x₁} {B'} k l)
--                        → (let l' = ex-btfs k l x in ∀ c' → ExistFirstBy (c' ≡_) ∘ toC  WitchIsAlso (B' c') l
--                                                           → ExistFirstBy (c' ≡_) ∘ toC  WitchIsAlso (B' c') l')
-- btf-Ex-property zero x c' x₁ = x₁
-- btf-Ex-property (suc k) x c' x₁ = {!x₁!}

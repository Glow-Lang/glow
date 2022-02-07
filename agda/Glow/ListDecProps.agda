{-# OPTIONS --cubical  #-}
module Glow.ListDecProps where

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
BTF [] = Unit
BTF (x ∷ xs) = BTF xs ⊎ BTF' (x ∷ xs) 

mapTail : ∀ {ℓ} {A : Type ℓ} → (List A → List A) → List A → List A
mapTail x [] = []
mapTail x (x₁ ∷ x₂) = x₁ ∷ x x₂

pick' :  ∀ {ℓ} {A : Type ℓ} → ∀ a → ∀ l → BTF' (a ∷ l) → A × List A
pick' x xs nothing = x , xs
pick' x (x₁ ∷ xs) (just y) = map-prod (idfun _) (x₁ ∷_) (pick' x xs y)
   

pick :  ∀ {ℓ} {A : Type ℓ} → ∀ l → BTF' l → List A
pick = {!!}

btf :  ∀ {ℓ} {A : Type ℓ} → (l : List A) → BTF l → List A
btf [] _ = []
btf (x ∷ xs) (inl x₁) = x ∷ btf xs x₁
btf (x ∷ xs) (inr x₁) = pick (x ∷ xs) x₁


-- BTF'-step : ∀ {ℓ} {A : Type ℓ} → (l : List A) → (bb : BTF' l) → BTF' (pick l bb)
-- BTF'-step (x ∷ l) nothing = nothing
-- BTF'-step (x ∷ l) (just x₁) = {!!}


-- BTF-step : ∀ {ℓ} {A : Type ℓ} → (l : List A) → (bb : BTF l) → BTF (btf l bb)
-- BTF-step [] bb = _
-- BTF-step (x ∷ l) (inl x₁) = inl (BTF-step l x₁)
-- BTF-step (x ∷ l) (inr x₁) = {!!}

-- btf-many : ∀ {ℓ} {A : Type ℓ} → (l : List A) → List (BTF l) → List A
-- btf-many = {!!}

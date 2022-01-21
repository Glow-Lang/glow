
{-# OPTIONS --cubical  #-}
module Glow.DecEqMore where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Unit
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim)
open import Cubical.Data.List
open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec)
open import Cubical.Foundations.CartesianKanOps

open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool renaming (if_then_else_ to if_then_else'_ ; _≟_ to _≟B_)

open import Cubical.Data.Nat.Order.Recursive renaming (_≟_ to _≟NOR_)

open import Cubical.Relation.Nullary renaming (¬_ to IsEmpty)

open import Cubical.HITs.SetQuotients

open import Cubical.HITs.Interval renaming (elim to interval-elim)

import Cubical.Functions.Logic as L

open import Cubical.Foundations.Univalence

infix 20 ??_ 



Empty-elim-dot : ∀ {w} {Whatever : Type w} → .⊥ → Whatever
Empty-elim-dot ()

-- recompute : ∀ {a} {A : Type a} → {{Dec A}} → .A → A
-- recompute {{yes x}} _ = x
-- recompute {{no ¬p}} x = Empty-elim-dot (¬p x)

fromDec : ∀ {ℓ} {A : Type ℓ} (Q : Dec A) → Type ℓ 
fromDec {A = A} _ = A

fromWitness : ∀ {ℓ} {A : Type ℓ} {Q : Dec A} → A → True Q 
fromWitness {Q = yes p} x = _
fromWitness {Q = no ¬p} x = ¬p x



??_ :  ∀ {ℓ} (A : Type ℓ) → {{Dec-A : Dec A}} → Dec A   
??_ A ⦃ Dec-A = Dec-A ⦄ = Dec-A


dec-rec : ∀ {ℓ ℓ'} (A : Type ℓ) {B : Type ℓ'} → {{Dec-A : Dec A}} →
             (A → B) → (IsEmpty A → B) → B 
dec-rec A x x₁ with ?? A
... | yes p = x p
... | no ¬p = x₁ ¬p

dec-rec' : ∀ {ℓ ℓ'} (A : Type ℓ) {B : Type ℓ'} →
             (A → B) → (IsEmpty A → B) → Dec A → B 
dec-rec' A x x₁ (yes p) = x p
dec-rec' A x x₁ (no ¬p) = x₁ ¬p

dec-elim : ∀ {ℓ ℓ'} {A : Type ℓ} (B : Dec A → Type ℓ') →
             (∀ x → B (yes x)) → (∀ x → B (no x)) → ∀ x → B x 
dec-elim B x x₁ (yes p) = x p
dec-elim B x x₁ (no ¬p) = x₁ ¬p

dec-elim2 : ∀ {ℓ ℓ'} {A A' : Type ℓ} (B : Dec A → Dec A' → Type ℓ') 
             → (∀ x x' → B (yes x) (yes x'))
             → (∀ x x' → B (yes x) (no x'))
            → (∀ x x' → B (no x) (yes x'))
             → (∀ x x' → B (no x) (no x'))
             → ∀ x x' → B x x' 
dec-elim2 B x x₁ x₂ x₃ (yes p) (yes p₁) = x p p₁
dec-elim2 B x x₁ x₂ x₃ (yes p) (no ¬p) = x₁ p ¬p
dec-elim2 B x x₁ x₂ x₃ (no ¬p) (yes p) = x₂ ¬p p
dec-elim2 B x x₁ x₂ x₃ (no ¬p) (no ¬p₁) = x₃ ¬p ¬p₁


Dec-≡ : ∀ {ℓ} {A : Type ℓ} {B : Type ℓ} → {{Dec-A : Dec A}} → {{Dec-B : Dec B}} →
          (p : A ≡ B) → True (Dec-A) ≡ True (Dec-B)  
Dec-≡ ⦃ Dec-A = yes p₁ ⦄ ⦃ yes p₂ ⦄ p = refl
Dec-≡ ⦃ Dec-A = yes p₁ ⦄ ⦃ no ¬p ⦄ p = empty-rec (¬p (transport p p₁))
Dec-≡ ⦃ Dec-A = no ¬p ⦄ ⦃ yes p₁ ⦄ p = empty-rec (¬p (transport⁻ p p₁))
Dec-≡ ⦃ Dec-A = no ¬p ⦄ ⦃ no ¬p₁ ⦄ p = refl

True-≡ : ∀ {ℓ} {A : Type ℓ} {B : Type ℓ} → {{Dec-A : Dec A}} → {{Dec-B : Dec B}} →
          (p : A ≡ B) → {x : True Dec-A} {y : True Dec-B} → PathP (λ i → Dec-≡ p i ) x y  
True-≡ ⦃ Dec-A = yes p₁ ⦄ ⦃ yes p₂ ⦄ p = refl

-- postulate True-Pa' : ∀ {ℓ} {A : I → Type ℓ} {Dec-A : ∀ i → Dec (A i)} →
--                       {x : True (Dec-A i0)} {y : True (Dec-A i1)}
--                       → Dec (A i0) → Dec (A i1) → PathP (λ i → True (Dec-A i)) x y  
-- -- True-Pa' {Dec-A = Dec-A} p {x} {y} (yes p₁) (yes p₂) = {!!}
-- -- True-Pa' {Dec-A = Dec-A} p {x} {y} (yes p₁) (no ¬p) = {!!}
-- -- True-Pa' {Dec-A = Dec-A} p {x} {y} (no ¬p) (yes p₁) = {!!}
-- -- True-Pa' {Dec-A = Dec-A} p {x} {y} (no ¬p) (no ¬p₁) = {!!}

-- True-Pa : ∀ {ℓ} {A : I → Type ℓ} {Dec-A : ∀ i → Dec (A i)} →
--           .{x : True (Dec-A i0)} .{y : True (Dec-A i1)}
--            → .(PathP (λ i → True (Dec-A i)) x y)  
-- True-Pa {Dec-A = Dec-A} = ? --True-Pa' {Dec-A = Dec-A} (Dec-A i0) (Dec-A i1)

record IsDiscrete {ℓ} (A : Type ℓ) : Type ℓ where
  field
    eqTest : Discrete A

open IsDiscrete public

_≟_ : ∀ {ℓ} {A : Type ℓ} → {{IsDiscrete-A : IsDiscrete A}} → Discrete A 
_≟_ ⦃ IsDiscrete-A = IsDiscrete-A ⦄ = IsDiscrete-A .eqTest

instance
  ℕ-Discrete : IsDiscrete ℕ
  eqTest ℕ-Discrete = discreteℕ 

instance
  Bool-Discrete : IsDiscrete Bool
  eqTest Bool-Discrete = _≟B_




instance
  ×-Discrete : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B : Type ℓ'} → {{IsDiscrete A}} → {{IsDiscrete B}} → IsDiscrete (A × B)   
  eqTest ×-Discrete (x , x₁) (x₂ , x₃) with x ≟ x₂ | x₁ ≟ x₃
  ... | yes p | yes p₁ = yes (×≡ p p₁)
  ... | yes _ | no ¬p = no (¬p ∘ cong proj₂)
  ... | no ¬p | yes _ = no (¬p ∘ cong proj₁)
  ... | no _ | no ¬p = no (¬p ∘ cong proj₂)

-- instance
--   ×-Discrete : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B : Type ℓ'} → {{IsDiscrete A}} → {{IsDiscrete B}} → IsDiscrete (A × B)   
--   eqTest ×-Discrete (x , x₁) (x₂ , x₃) with x ≟ x₂ | x₁ ≟ x₃
--   ... | yes p | yes p₁ = yes (×≡ p p₁)
--   ... | yes _ | no ¬p = no (¬p ∘ cong proj₂)
--   ... | no ¬p | yes _ = no (¬p ∘ cong proj₁)
--   ... | no _ | no ¬p = no (¬p ∘ cong proj₂)


instance
  discrete-→-≡-dec : ∀ {ℓ} {A : Type ℓ} → {{IsDiscrete-A : IsDiscrete A}} → {x y : A} → Dec (x ≡ y)
  discrete-→-≡-dec = _ ≟ _ 


instance
  ×-Dec : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B : Type ℓ'} → {{Dec-A : Dec A}} → {{Dec-B : Dec B}} → Dec (A × B)   
  ×-Dec {{Dec-A}} {{Dec-B}} with Dec-A | Dec-B
  ... | yes p | yes p₁ = yes (p , p₁)
  ... | yes _ | no ¬p = no (¬p ∘ proj₂)
  ... | no ¬p | yes _ = no (¬p ∘ proj₁)
  ... | no _ | no ¬p = no (¬p ∘ proj₂)

instance
  ⊎-Dec : ∀ {ℓ} → {A : Type ℓ} → {B : Type ℓ} → {{Dec A}} → {{Dec B}} → Dec (A ⊎ B)   
  ⊎-Dec {{Dec-A}} {{Dec-B}} = _⊎?_ Dec-A Dec-B

instance
  Bool→Type-Dec : ∀ {b} → Dec (Bool→Type b)   
  Bool→Type-Dec {false} = no (idfun _)
  Bool→Type-Dec {true} = yes tt





-- zzz : ℕ → Unit
-- zzz x with ?? (_≡_ {A = ℕ × ℕ} (x , 3) (suc x , 2))
-- ... | yes p = {!!}
-- ... | no ¬p = {!!}




primStringEquality-comm : ∀ {x y} → primStringEquality x y ≡ primStringEquality y x
primStringEquality-comm {x} {y} with primStringEquality x y | primStringEquality y x
... | false | false = refl
... | false | true = imposible-primStringEquality-comm
  where
    postulate imposible-primStringEquality-comm : _ 
... | true | false = imposible-primStringEquality-comm
  where
    postulate imposible-primStringEquality-comm : _
... | true | true = refl


postulate same-strings :
             ∀ {x y : String} →
             Bool→Type (primStringEquality x y) → x ≡ y

postulate different-strings :
             ∀ {x y : String} →
             Bool→Type (not (primStringEquality x y)) → IsEmpty (x ≡ y)

dichotomyBool' : (x : Bool) → (Bool→Type x) ⊎ (Bool→Type (not x))
dichotomyBool' false = inr _
dichotomyBool' true = inl _

-- instance
String-Discrete-postulated : IsDiscrete String
eqTest String-Discrete-postulated x y = 

   sum-elim {C = const (Dec (x ≡ y)) }
      (yes ∘ same-strings {x} {y})
      (no ∘ different-strings {x} {y})
   (dichotomyBool' (primStringEquality x y))


String' : Type₀
String' = String / λ x x₁ → Bool→Type (primStringEquality x x₁)




-- castAA : ∀ {s} → AA s → Maybe (AA _/_.[ "zz" ] )
-- castAA {s} x with ?? (s ≡ _/_.[ "zz" ])
-- ... | yes p = just (subst AA p x)
-- ... | no ¬p = nothing

-- castAATest : Maybe (AA _/_.[ "zz" ])
-- castAATest = castAA {_/_.[ "zz" ]} (record { zz = 3 })

-- castAATest= : castAATest ≡ just (record { zz = 3 }) 
-- castAATest= = cong just (isSet-subst {B = AA} squash/ (eq/ _ _ _) (record { zz = 3 })) 

no-dec-eq-help : ∀ {ℓ} → {A : Type ℓ} → {t : A → Bool} → {x y : A}
                    → False ((t x) ≟B (t y))
                    → Dec (x ≡ y)
no-dec-eq-help {t = t} {x} {y} z = no ((toWitnessFalse z) ∘ cong t) 

no-dec-eq-help' : ∀ {ℓ} → {A : Type ℓ} → (t : A → A → Bool) → {x y : A}
                    → False ((t x x) ≟B (t x y))
                    → Dec (x ≡ y)
no-dec-eq-help' t {x} {y} z = no-dec-eq-help {t = t x} {x} {y} z  

record Dec-Pred  {ℓ ℓ'} {A : Type ℓ} (B : A → Type ℓ') : Type (ℓ-max ℓ ℓ') where
  constructor dec-pred
  field
    decide : ∀ a → Dec (B a) 

-- not instance since Dec class is to general, and such instance seems to caus too much problems with instance resolution
-- instance
Pred-app : ∀ {ℓ ℓ'} {A : Type ℓ} {B : A → Type ℓ'} {{Dec-Pred-B : Dec-Pred B}} {a : A} → Dec (B a)
Pred-app {ℓ} {ℓ'} {A} {B} ⦃ Dec-Pred-B ⦄ {a} = Dec-Pred.decide Dec-Pred-B a

Pred-app' : ∀ {ℓ ℓ'} {A : Type ℓ} {B : A → Type ℓ'} {{Dec-Pred-B : Dec-Pred B}} (a : A) → Dec (B a)
Pred-app' {ℓ} {ℓ'} {A} {B} ⦃ Dec-Pred-B ⦄ a = Dec-Pred.decide Dec-Pred-B a


instance
  Dec-Pred-Disc : ∀ {ℓ ℓ'} {A : Type ℓ} {A' : Type ℓ'} {f : A → A'} {{IsDiscrete-A : IsDiscrete A'}} {a' : A'} → Dec-Pred ((a' ≡_) ∘ f)
  Dec-Pred.decide Dec-Pred-Disc a = ?? _


Dec-Pred-Maybe : ∀ {ℓ ℓ'} {A : Type ℓ} {A' : Type ℓ} {f : A → Maybe A'} {B-n : Type ℓ'} {B-j : A' → Type ℓ'} {{Dec-B-n : Dec B-n}} {{Dec-B-j : Dec-Pred B-j}}
                      → Dec-Pred ((recMaybe B-n B-j) ∘ f)
Dec-Pred.decide (Dec-Pred-Maybe {f = f} {{Dec-B-j = Dec-B-j}}) a with f a
... | nothing = ?? _
... | just x = Pred-app {{Dec-B-j}} {x}


-- Dec-Pred-Disc : ∀ {ℓ} {A : Type ℓ} {{IsDiscrete-A : IsDiscrete A}} {a : A} → Dec-Pred (a ≡_)
-- Dec-Pred.predicateDecision Dec-Pred-Disc a = ?? _

instance
  Dec-IsEmpty : ∀ {ℓ} {A : Type ℓ} {{Dec-A : Dec A}} → Dec (IsEmpty A)
  Dec-IsEmpty {ℓ} {A} ⦃ yes p ⦄ = no λ x → x p
  Dec-IsEmpty {ℓ} {A} ⦃ no ¬p ⦄ = yes ¬p

instance
  Dec-Empty : ∀ {ℓ} → Dec (Lift {ℓ-zero} {ℓ} ⊥)
  Dec-Empty = no lower



Id-Dec-hlp→ : ∀ {ℓ ℓ'} {A : Type ℓ} {A' : Type ℓ'} {A'' : Type ℓ'} {{Dec-A : Dec A}} →
               (dec-rec A (const A') (const A'')) → (A × A') ⊎ (IsEmpty A × A'') 
Id-Dec-hlp→ ⦃ Dec-A = yes p ⦄ x = inl (p , x)
Id-Dec-hlp→ ⦃ Dec-A = no ¬p ⦄ x = inr (¬p , x)

Id-Dec-hlp← : ∀ {ℓ ℓ'} {A : Type ℓ} {A' : Type ℓ'} {A'' : Type ℓ'} {{Dec-A : Dec A}} →
               (A × A') ⊎ (IsEmpty A × A'') → (dec-rec A (const A') (const A'')) 
Id-Dec-hlp← ⦃ Dec-A = yes p ⦄ (inl x) = proj₂ x
Id-Dec-hlp← ⦃ Dec-A = yes p ⦄ (inr x) = empty-elim (proj₁ x p)
Id-Dec-hlp← ⦃ Dec-A = no ¬p ⦄ (inl x) = empty-elim (¬p (proj₁ x))
Id-Dec-hlp← ⦃ Dec-A = no ¬p ⦄ (inr x) = proj₂ x


Id-Dec-hlp-mid : ∀ {ℓ ℓ' ℓ''} {A : Type ℓ} {A' : Type ℓ'} {A'' : Type ℓ'}  {B' : Type ℓ''} {B'' : Type ℓ''} {{Dec-A : Dec A}} →
                  (A → A' → B') → (IsEmpty A → A'' → B'') → (dec-rec A (const A') (const A'')) → (dec-rec A (const B') (const B'')) 
Id-Dec-hlp-mid ⦃ Dec-A = yes p ⦄ f g = f p 
Id-Dec-hlp-mid ⦃ Dec-A = no ¬p ⦄ f g = g ¬p

Id-Dec-hlp-mid' : ∀ {ℓ ℓ2 ℓ' ℓ''} {A : Type ℓ} {A2 : Type ℓ2} {A' : Type ℓ'} {A'' : Type ℓ'}  {B' : Type ℓ''} {B'' : Type ℓ''} {{Dec-A : Dec A}} {{Dec-A2 : Dec A2}} →
                   (A → A2) → (A2 → A) → (A → A' → B') → (IsEmpty A → A'' → B'') → (dec-rec A (const A') (const A'')) → (dec-rec A2 (const B') (const B'')) 
Id-Dec-hlp-mid' ⦃ Dec-A = yes p ⦄ ⦃ yes p₁ ⦄ _ _ f _ = f p
Id-Dec-hlp-mid' ⦃ Dec-A = yes p ⦄ ⦃ no ¬p ⦄ y _ _ _ _ = empty-elim (¬p (y p))
Id-Dec-hlp-mid' ⦃ Dec-A = no ¬p ⦄ ⦃ yes p ⦄ _ n _ _ _ = empty-elim (¬p (n p))
Id-Dec-hlp-mid' ⦃ Dec-A = no ¬p ⦄ ⦃ no ¬p₁ ⦄ _ n _ g = g ¬p

Id-Dec-hlp : ∀ {ℓ ℓ2 ℓ' ℓ''} {A : Type ℓ} {A2 : Type ℓ2} {A' : Type ℓ'} {A'' : Type ℓ'}  {B' : Type ℓ''} {B'' : Type ℓ''} {{Dec-A : Dec A}} {{Dec-A2 : Dec A2}} →
                   (A → A2) → (A2 → A) → (A → A' → B') → (IsEmpty A → A'' → B'') → (A × A') ⊎ (IsEmpty A × A'') → (A2 × B') ⊎ (IsEmpty A2 × B'') 
Id-Dec-hlp y n f g x = Id-Dec-hlp→ (Id-Dec-hlp-mid' y n f g (Id-Dec-hlp← x))

-- test diferent variants of pattern amtching to figure out best evaluation
isProp-True : ∀ {ℓ} {A : Type ℓ} {Q : Dec A} → isProp (True Q)
isProp-True {Q = yes p} = isPropUnit
isProp-True {Q = no ¬p} = isProp⊥

-- record IsDecProp ℓ : Type ? where


-- record DecProp ℓ : Type ?

DecPropΣ : Type₁
DecPropΣ = Σ Type₀ (λ x → Dec x × isProp x)

module PropMode (b : Interval) where


  PM-h : ∀ A → (Dec A × isProp A) → Interval → Σ Type₀ (A ≡_ ) 
  PM-h A x zero = (True (proj₁ x)) , hPropExt (proj₂ x) isProp-True fromWitness toWitness
  PM-h A x one = A , (λ _ → A)
  PM-h A x (seg i) = snd (isContrSingl A) ((True (proj₁ x)) , hPropExt (proj₂ x) isProp-True fromWitness toWitness) (~ i)


  -- toWitness'-P' : ∀ {A x} → _≡_ {A = Interval → Σ Type₀ (A ≡_ )} (PM-h A x) λ x₁ → _ , refl --PathP (λ i → fst (PM-h A x (seg i)) → A) toWitness (idfun _)
  -- toWitness'-P' {A} {x} = isPropΠ (λ _ → isContr→isProp (isContrSingl _)) _ _ 


  toWitness'-P : ∀ {A x} → PathP (λ i → fst (PM-h A x (seg i)) → A) toWitness (idfun _)
  toWitness'-P {A} {x} = toPathP (isPropΠ (const (proj₂ x)) _ _)

  fromWitness'-P : ∀ {A x} → PathP (λ i → fst (PM-h A x (seg i)) → True (proj₁ x)) (idfun _) fromWitness 
  fromWitness'-P {A} {x} = toPathP (isPropΠ (const isProp-True) _ _)


  toWitness'-h' : ∀ {A x} → ∀ b → fst (PM-h A x b) → A
  toWitness'-h' {A} zero = toWitness
  toWitness'-h' {A} one = idfun _
  toWitness'-h' {A} {x} (seg i) = toWitness'-P {x = x} i

  fromWitness'-h' : ∀ {A x} → ∀ b → fst (PM-h A x b) → True (proj₁ x)
  fromWitness'-h' {A} zero = idfun _
  fromWitness'-h' {A} one = fromWitness
  fromWitness'-h' {A} {x} (seg i) = fromWitness'-P {x = x} i


  PM : DecPropΣ → Type₀
  PM x = fst (PM-h (fst x) (snd x) b)


  toWitness' : ∀ {A} → PM A → fst A
  toWitness' {A} = toWitness'-h' {fst A} {snd A} b

  fromWitness' : ∀ {A} → PM A → True (proj₁ (snd A))
  fromWitness' {A} = fromWitness'-h' {fst A} {snd A} b



⊎-isProp : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B : Type ℓ'} → isProp A → isProp B → (A → B → ⊥) → isProp (A ⊎ B)
⊎-isProp x x₁ y (inl x₃) (inl x₄) = cong inl (x _ _)
⊎-isProp x x₁ y (inl x₃) (inr x₄) = empty-elim (y x₃ x₄)
⊎-isProp x x₁ y (inr x₃) (inl x₄) = empty-elim (y x₄ x₃)
⊎-isProp x x₁ y (inr x₃) (inr x₄) = cong inr (x₁ _ _)

-- elimMaybe-Empty-isProp :
--       ∀ {ℓ ℓ'} {A : Type ℓ} {B : A → Type ℓ'} →
--       (∀ x → isProp (B x)) → ∀ x → isProp ((recMaybe ⊥ {!B!}) x)
-- elimMaybe-Empty-isProp = {!!}


recMaybe-Empty-isProp :
      ∀ {ℓ} {A : Type ℓ} {B : A → Type₀} →
      (∀ x → isProp (B x)) → ∀ x → isProp ((recMaybe ⊥ B) x)
recMaybe-Empty-isProp x (just x₁) = x _


Unit-dp : DecPropΣ
Unit-dp = Unit , ((yes tt) , (λ x y i → tt))

Empty-dp : DecPropΣ
Empty-dp = (⊥ , no (idfun _) , isProp⊥)


×-dp : DecPropΣ → DecPropΣ → DecPropΣ
×-dp x x₁ = (fst x × fst x₁ ) , (×-Dec {{proj₁ (snd x)}} {{proj₁ (snd x₁)}}
                  , λ x₂ y → ×≡ (proj₂ (snd x) _ _ ) (proj₂ (snd x₁) _ _))




Empty⊎ : ∀ {ℓ ℓ'} → {A : Type ℓ} → {B : Type ℓ'} → (IsEmpty A) → A ⊎ B → B
Empty⊎ x (inl x₁) = empty-elim (x x₁)
Empty⊎ x (inr x₁) = x₁

safeHead : {A : Type₀} → List A → Maybe A
safeHead [] = nothing
safeHead (x ∷ x₁) = just x

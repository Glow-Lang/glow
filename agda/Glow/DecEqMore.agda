
{-# OPTIONS --cubical  #-}
module Glow.DecEqMore where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List
open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec)


open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool renaming (if_then_else_ to if_then_else'_ ; _≟_ to _≟B_)

open import Cubical.Data.Nat.Order.Recursive renaming (_≟_ to _≟NOR_)

open import Cubical.Relation.Nullary renaming (¬_ to IsEmpty)

open import Cubical.HITs.SetQuotients

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


postulate same-strings : ∀ {x y : String} → x ≡ y


-- instance
String-Discrete-postulated : IsDiscrete String
eqTest String-Discrete-postulated x y with primStringEquality x y
... | false = no different-strings
  where 
    postulate different-strings : IsEmpty (x ≡ y) 
... | true = yes same-strings


String' : Type₀
String' = String / λ x x₁ → Bool→Type (primStringEquality x x₁)




record AA (s : String') : Type₀ where
  field
    zz : ℕ

-- instance
Dec-test-/ : ∀ {ℓ} → {A : Type ℓ} → (t : A → A → Bool) → IsDiscrete (A / λ x x₁ → (Bool→Type (t x x₁)))   
eqTest (Dec-test-/ t) =
  elimProp2 (λ _ _ → isPropDec (squash/ _ _) ) h

  where
    h : ∀ a b → Dec (_/_.[ a ] ≡ _/_.[ b ])
    h a b = mapDec
      (eq/ _ _)
      different-strings
      (?? (Bool→Type (t a b)))

      where
        postulate different-strings : IsEmpty (Bool→Type (t a b)) → IsEmpty (_/_.[ a ] ≡ _/_.[ b ])
         

instance
  String-Discrete : IsDiscrete String'
  String-Discrete = Dec-test-/ primStringEquality



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



module PropMode (b : Bool) where

  PM-h : Bool → Σ _ Dec → Type₀
  PM-h false = True ∘ snd
  PM-h true = fst

  
  PM : Σ _ Dec → Type₀
  PM = PM-h b 
    

  toWitness'-h : ∀ {A} → ∀ b → PM-h b A → fst A
  toWitness'-h {A} false x = toWitness x
  toWitness'-h true x = x

  fromWitness'-h : ∀ {A} → ∀ b → fst A → PM-h b A
  fromWitness'-h {fst₁ , _} false x = fromWitness x
  fromWitness'-h true x = x


  toWitness' : ∀ {A} → PM A → fst A
  toWitness' = toWitness'-h b

  fromWitness' : ∀ {A} → fst A → PM A
  fromWitness' = fromWitness'-h b



{-# OPTIONS --cubical  #-}
module Glow.ASTSafe where

open import Agda.Builtin.String
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List

open import Cubical.Data.Maybe renaming (rec to recMaybe ;  nothing to ◦)
open import Cubical.Data.Bool hiding (if_then_else_)

open import Cubical.Data.Nat.Order.Recursive
open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

infixr 80 •_

pattern • x = just x

data TopLevelDefinition : Type₀

Module = List TopLevelDefinition

data GType : Type₀ where
  Boolᵍ : GType 
  Intᵍ : GType
  Natᵍ : GType

GTypeAgdaRep : GType → Type₀
GTypeAgdaRep Boolᵍ = Bool
GTypeAgdaRep Intᵍ = ℤ
GTypeAgdaRep Natᵍ = ℕ


data InteractionBody {participantsℕ : ℕ} {paramtersTy : List GType} : Type₀

data InteractionEffect {participantsℕ : ℕ} {paramtersTy : List GType} (ib : InteractionBody {participantsℕ} {paramtersTy}) : Type₀

data InteractionPart {participantsℕ : ℕ} {paramtersTy : List GType} (ib : InteractionBody {participantsℕ} {paramtersTy}) : Type₀

data InteractionValue {participantsℕ : ℕ} {paramtersTy : List GType} (ib : InteractionBody {participantsℕ} {paramtersTy}) GType : Type₀


-- data InteractionExpression {participantsℕ : ℕ} {paramtersTy : List GType} : Type₀




getFreeSymbol : {participantsℕ : ℕ} {paramtersTy : List GType}
                        → InteractionBody {participantsℕ} {paramtersTy} → ℕ

Disc→Ty : ∀ {ℓ} → ∀ {A : Type ℓ} → Dec A → Type₀
Disc→Ty (yes p) = ⟨ ⊤ ⟩
Disc→Ty (no ¬p) = ⟨ ⊥ ⟩

=ℕTy : ℕ → ℕ → Type₀
=ℕTy x x₁ = Disc→Ty (discreteℕ x x₁)

forceFreeSymbol : {participantsℕ : ℕ} {paramtersTy : List GType}
                        → InteractionBody {participantsℕ} {paramtersTy} → ℕ → Type₀
forceFreeSymbol ib k = k ≤ getFreeSymbol ib

isDefinedSymbol : {participantsℕ : ℕ} {paramtersTy : List GType}
                        → InteractionBody {participantsℕ} {paramtersTy} → ℕ → Type₀
isDefinedSymbol ib k = k < getFreeSymbol ib

isDefinedSymbolOfTy : {participantsℕ : ℕ} {paramtersTy : List GType}
                        → InteractionBody {participantsℕ} {paramtersTy} → ℕ → GType → Type₀
isDefinedSymbolOfTy ib k _ = ⟨ ⊥ ⟩

isParticipantSymbol : {participantsℕ : ℕ} {paramtersTy : List GType}
                        → InteractionBody {participantsℕ} {paramtersTy} → ℕ → Type₀
isParticipantSymbol {participantsℕ = pN} _  k = k < pN


statementScopeCheck : {participantsℕ : ℕ} {paramtersTy : List GType}
                        → (ib : InteractionBody {participantsℕ} {paramtersTy}) → InteractionPart {participantsℕ} {paramtersTy} ib →  Type₀
statementScopeCheck ib ip = ⟨ ⊤ ⟩ 

infixl 6 interaction⟨_,_⟩⁅_⁆

infix 60 _ₗ
infix 60 _ₗ'



data TopLevelDefinition where
  interaction⟨_,_⟩⁅_⁆ :
     (participantsℕ : ℕ) →
     (paramtersTy : List GType) →
     InteractionBody {participantsℕ} {paramtersTy} →
       TopLevelDefinition

-- forceLetSymbol : ℕ 

data InteractionEffect ib where
  -- verify!  : {!!} → InteractionEffect ib
  publish!_⟶_ : (k : ℕ) → {_ : isParticipantSymbol ib k} →
                    (j : ℕ) → {_ : isDefinedSymbol ib j} → InteractionEffect ib
  deposit!_⟶_  : (k : ℕ) → {_ : isParticipantSymbol ib k}
                   → InteractionValue ib Intᵍ
                   → InteractionEffect ib
  withdraw!_⟵_ : (k : ℕ) → {_ : isParticipantSymbol ib k}
                   → InteractionValue ib Intᵍ
                   → InteractionEffect ib

infix 40 _-_∶_≔_
-- infixl 40 ↯_

data InteractionPart {participantsℕ} {paramtersTy} ib where
  _-_∶_≔_ : (j : Maybe ℕ) → {_ : recMaybe  ⟨ ⊤ ⟩ ( isParticipantSymbol ib) j } →
               (k : ℕ) → (gTy : GType) → {_ : forceFreeSymbol ib k} →  InteractionValue ib gTy
                 → InteractionPart ib
  ↯_ : InteractionEffect ib → InteractionPart ib
  if_then_else_ : InteractionValue ib Boolᵍ →
                    InteractionBody {participantsℕ} {paramtersTy} →
                    InteractionBody {participantsℕ} {paramtersTy} → InteractionPart ib
  
-- let'_∶_≡_ : {participantsℕ : ℕ} {paramtersTy : List GType}
--                         → {ib : InteractionBody {participantsℕ} {paramtersTy}} →                
--                (k : ℕ) → (gTy : GType) → {_ : forceFreeSymbol ib k} →  InteractionValue ib gTy
--                  → InteractionPart ib
-- let'_∶_≡_ = (_letᵍ_∶_≡_) nothing

-- ⒜_let'_∶_≡_ : {participantsℕ : ℕ} {paramtersTy : List GType}
--                         → {ib : InteractionBody {participantsℕ} {paramtersTy}} →
--                (j : ℕ) → {_ : isParticipantSymbol ib j } →
--                (k : ℕ) → (gTy : GType) → {_ : forceFreeSymbol ib k} →  InteractionValue ib gTy
--                  → InteractionPart ib
-- ⒜_let'_∶_≡_ {participantsℕ} {paramtersTy} {ib} j {x} k gTy {x₁} x₂ =
--   _letᵍ_∶_≡_ {participantsℕ} {paramtersTy} {ib} (• j) {x} k gTy {x₁} x₂



record IsGlowTy {a} (A : Type a) : Type a where
  field
    glowRep : GType
    cast : A → GTypeAgdaRep glowRep


data InteractionValue {participantsℕ} {paramtersTy} ib gTy where
  𝓵_ : (k : ℕ) → {_ : isDefinedSymbolOfTy ib k gTy} → InteractionValue ib gTy
  _ₗ' : GTypeAgdaRep gTy → InteractionValue ib gTy
  input : String → InteractionValue ib gTy
  -- 𝓹 : (k : ℕ) → {_ : isParticipantSymbol ib k} →  InteractionValue ib gTy

_ₗ : {participantsℕ : ℕ} {paramtersTy : List GType} → 
      {ib : InteractionBody {participantsℕ} {paramtersTy}} → 
                        {A : Type₀} → {{isGlowTy : IsGlowTy A}} →
                        A →  InteractionValue ib (IsGlowTy.glowRep isGlowTy)
_ₗ {participantsℕ} {paramtersTy} {ib} {A} ⦃ isGlowTy ⦄ x = IsGlowTy.cast isGlowTy x ₗ' 

instance
  Bool-IsGlowTy : IsGlowTy Bool
  Bool-IsGlowTy = record { glowRep = Boolᵍ ; cast = idfun _}

instance
  ℤ-IsGlowTy : IsGlowTy ℤ
  ℤ-IsGlowTy = record { glowRep = Intᵍ  ; cast = idfun _ }

instance
  ℕ-IsGlowTy : IsGlowTy ℕ
  ℕ-IsGlowTy = record { glowRep = Natᵍ  ; cast = idfun _ }



infixl 15 _；_
infix 17 _；₁

data InteractionBody {participantsℕ} {paramtersTy} where
 ∙ib : InteractionBody {participantsℕ} {paramtersTy}
 _；_ : (ss : InteractionBody {participantsℕ} {paramtersTy})
         →  (s : InteractionPart ss) → InteractionBody

pattern _；₁ x = ∙ib ； x  

-- data InteractionExpression {participantsℕ} {paramtersTy} where

getFreeSymbol {paramtersTy = l} ∙ib = length l
getFreeSymbol (x ； (_ - x₁ ∶ _ ≔ x₂)) = suc (getFreeSymbol x)
getFreeSymbol (x ； _) = getFreeSymbol x


-- testModule : Module
-- testModule =
--   interaction⟨ 2 , Boolᵍ ∷ [] ⟩⁅
--      ∙ib ；
--      ◦ - 1 ∶ Boolᵍ ≔ false ₗ ；
--      ◦ - 1 ∶ Natᵍ ≔ 3 ₗ ；
--      {! !} ；
--      {!!}
--   ⁆

--     ∷ []


boolGameModule : Module
boolGameModule =
  interaction⟨ 2 , Boolᵍ ∷ [] ⟩⁅ ∙ib ；
  
      ↯ deposit! 0 ⟶ 1 ₗ ；
      ↯ deposit! 1 ⟶ 1 ₗ ；

      

      • 0 - 1 ∶ Boolᵍ ≔ input "Enter A's choice." ；
      ↯ publish! 0 ⟶ 1 ；
      
      • 1 - 2 ∶ Boolᵍ ≔ input "Enter B's choice." ；
      ↯ publish! 1 ⟶ 2 ；

      ◦ - {!!} ∶ {!!}  ≔ {!!} ；
      
      if 𝓵 {!!}  
        then (↯ withdraw! 0 ⟵ 2 ₗ ；₁)
        else (↯ withdraw! 0 ⟵ 2 ₗ ；₁)
  ⁆

    ∷ []

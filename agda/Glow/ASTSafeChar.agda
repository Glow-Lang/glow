
{-# OPTIONS --cubical  #-}
module Glow.ASTSafeChar where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List


open import Cubical.Data.Maybe renaming (rec to recMaybe ;  nothing to ◦)
open import Cubical.Data.Bool renaming (if_then_else_ to if_then_else'_)

open import Cubical.Data.Nat.Order.Recursive
open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

infixr 80 •

pattern • x = just x

data TopLevelDefinition : Type₀

Module = List TopLevelDefinition

data GType : Type₀ where
  Boolᵍ : GType 
  Intᵍ : GType
  Natᵍ : GType

-- GTy≟ : Discrete GType
-- GTy≟ x y = {!x y!}

GTy== : GType → GType → Bool
GTy== Boolᵍ Boolᵍ = true
GTy== Intᵍ Intᵍ = true
GTy== Natᵍ Natᵍ = true
GTy== _ _ = false


GTypeAgdaRep : GType → Type₀
GTypeAgdaRep Boolᵍ = Bool
GTypeAgdaRep Intᵍ = ℤ
GTypeAgdaRep Natᵍ = ℕ


data InteractionBody {participants : List Char} {paramtersTy : List GType} : Type₀

data InteractionEffect {participants : List Char} {paramtersTy : List GType} (ib : InteractionBody {participants} {paramtersTy}) : Type₀

data InteractionPart {participants : List Char} {paramtersTy : List GType} (ib : InteractionBody {participants} {paramtersTy}) : Type₀

data InteractionValue {participants : List Char} {paramtersTy : List GType} (ib : InteractionBody {participants} {paramtersTy}) GType : Type₀


-- data InteractionExpression {participants : List Char} {paramtersTy : List GType} : Type₀




getFreeSymbol : {participants : List Char} {paramtersTy : List GType}
                        → InteractionBody {participants} {paramtersTy} → ℕ

Disc→Ty : ∀ {ℓ} → ∀ {A : Type ℓ} → Dec A → Type₀
Disc→Ty (yes p) = ⟨ ⊤ ⟩
Disc→Ty (no ¬p) = ⟨ ⊥ ⟩

=ℕTy : ℕ → ℕ → Type₀
=ℕTy x x₁ = Disc→Ty (discreteℕ x x₁)

forceFreeSymbol : {participants : List Char} {paramtersTy : List GType}
                        → InteractionBody {participants} {paramtersTy} → ℕ → Type₀
forceFreeSymbol ib k = k ≤ getFreeSymbol ib

isDefinedSymbol : {participants : List Char} {paramtersTy : List GType}
                        → InteractionBody {participants} {paramtersTy} → ℕ → Type₀
isDefinedSymbol ib k = k < getFreeSymbol ib

getSymbolTy : {participants : List Char} {paramtersTy : List GType}
                        → InteractionBody {participants} {paramtersTy} → ℕ → Maybe GType



isDefinedSymbolOfTy : {participants : List Char} {paramtersTy : List GType}
                        → InteractionBody {participants} {paramtersTy} → ℕ → GType → Type₀
isDefinedSymbolOfTy ib k ty = 
  recMaybe ⟨ ⊥ ⟩ (λ x → if (GTy== ty x) then ⟨ ⊤ ⟩ else' ⟨ ⊥ ⟩) (getSymbolTy ib k)

MemberBy : ∀ {ℓ} → {A : Type ℓ} → (A → A → Bool) → List A → A → Type₀
MemberBy f [] _ = ⟨ ⊥ ⟩
MemberBy f (x' ∷ xs) x =
  if f x x'
   then ⟨ ⊤ ⟩
   else' MemberBy f xs x
   
isParticipantSymbol : {participants : List Char} {paramtersTy : List GType}
                        → InteractionBody {participants} {paramtersTy} → Char → Type₀
isParticipantSymbol {participants = p} _ k = MemberBy primCharEquality p k 


statementScopeCheck : {participants : List Char} {paramtersTy : List GType}
                        → (ib : InteractionBody {participants} {paramtersTy}) → InteractionPart {participants} {paramtersTy} ib →  Type₀
statementScopeCheck ib ip = ⟨ ⊤ ⟩ 

infixl 6 interaction⟨_,_⟩⁅_⁆

infix 60 _ₗ
infix 60 _ₗ'
infix 60 𝓵_



data TopLevelDefinition where
  interaction⟨_,_⟩⁅_⁆ :
     (participants : List Char) →
     (paramtersTy : List GType) →
     InteractionBody {participants} {paramtersTy} →
       TopLevelDefinition

-- forceLetSymbol : ℕ 

data InteractionEffect ib where
  -- verify!  : {!!} → InteractionEffect ib
  publish!_⟶_ : (k : Char) → {_ : isParticipantSymbol ib k} →
                    (j : ℕ) → {_ : isDefinedSymbol ib j} → InteractionEffect ib
  deposit!_⟶_  : (k : Char) → {_ : isParticipantSymbol ib k}
                   → InteractionValue ib Intᵍ
                   → InteractionEffect ib
  withdraw!_⟵_ : (k : Char) → {_ : isParticipantSymbol ib k}
                   → InteractionValue ib Intᵍ
                   → InteractionEffect ib

infix 40 _-_∶_≔_
infix 20 ↯_

data InteractionPart {participants} {paramtersTy} ib where
  _-_∶_≔_ : (j : Maybe Char) → {_ : recMaybe  ⟨ ⊤ ⟩ ( isParticipantSymbol ib) j } →
               (k : ℕ) → (gTy : GType) → {_ : forceFreeSymbol ib k} →  InteractionValue ib gTy
                 → InteractionPart ib
  ↯_ : InteractionEffect ib → InteractionPart ib
  if_then_else_ : InteractionValue ib Boolᵍ →
                    InteractionBody {participants} {paramtersTy} →
                    InteractionBody {participants} {paramtersTy} → InteractionPart ib
  
-- let'_∶_≡_ : {participants : List Char} {paramtersTy : List GType}
--                         → {ib : InteractionBody {participants} {paramtersTy}} →                
--                (k : ℕ) → (gTy : GType) → {_ : forceFreeSymbol ib k} →  InteractionValue ib gTy
--                  → InteractionPart ib
-- let'_∶_≡_ = (_letᵍ_∶_≡_) nothing

-- ⒜_let'_∶_≡_ : {participants : List Char} {paramtersTy : List GType}
--                         → {ib : InteractionBody {participants} {paramtersTy}} →
--                (j : ℕ) → {_ : isParticipantSymbol ib j } →
--                (k : ℕ) → (gTy : GType) → {_ : forceFreeSymbol ib k} →  InteractionValue ib gTy
--                  → InteractionPart ib
-- ⒜_let'_∶_≡_ {participants} {paramtersTy} {ib} j {x} k gTy {x₁} x₂ =
--   _letᵍ_∶_≡_ {participants} {paramtersTy} {ib} (• j) {x} k gTy {x₁} x₂



record IsGlowTy {a} (A : Type a) : Type a where
  field
    glowRep : GType
    cast : A → GTypeAgdaRep glowRep

infix 50 _==_

data InteractionValue {participants} {paramtersTy} ib gTy where
  𝓵_ : (k : ℕ) → {_ : isDefinedSymbolOfTy ib k gTy} → InteractionValue ib gTy
  _ₗ' : GTypeAgdaRep gTy → InteractionValue ib gTy
  input : String → InteractionValue ib gTy
  _==_ : InteractionValue ib gTy → InteractionValue ib gTy → InteractionValue ib gTy
  -- 𝓹 : (k : ℕ) → {_ : isParticipantSymbol ib k} →  InteractionValue ib gTy

_ₗ : {participants : List Char} {paramtersTy : List GType} → 
      {ib : InteractionBody {participants} {paramtersTy}} → 
                        {A : Type₀} → {{isGlowTy : IsGlowTy A}} →
                        A →  InteractionValue ib (IsGlowTy.glowRep isGlowTy)
_ₗ {participants} {paramtersTy} {ib} {A} ⦃ isGlowTy ⦄ x = IsGlowTy.cast isGlowTy x ₗ' 

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
infixl 15 _；'_

data InteractionBody {participants} {paramtersTy} where
 ∙ib : InteractionBody {participants} {paramtersTy}
 _；_ : (ss : InteractionBody {participants} {paramtersTy})
         →  (s : InteractionPart ss) → InteractionBody

pattern _；₁ x = ∙ib ； x  

pattern _；'_ x y = ∙ib ； x ； y  

-- data InteractionExpression {participants} {paramtersTy} where

getFreeSymbol {paramtersTy = l} ∙ib = length l
getFreeSymbol (x ； (_ - _ ∶ _ ≔ _)) = suc (getFreeSymbol x)
getFreeSymbol (x ； _) = getFreeSymbol x

getSymbolTy ∙ib k = ◦
getSymbolTy (ib ； _ - k' ∶ gTy ≔ x) k with discreteℕ k k'
... | yes p = • gTy
... | no ¬p = getSymbolTy ib k
getSymbolTy (ib ； _) k = getSymbolTy ib k

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
  interaction⟨  'A' ∷ 'B' ∷ [] , Boolᵍ ∷ [] ⟩⁅ ∙ib ； 
      
      ↯ deposit! 'A' ⟶ 1 ₗ ； 
      ↯ deposit! 'B' ⟶ 1 ₗ ；
      
      • 'A' - 1 ∶ Boolᵍ ≔ input "Enter A's choice." ；
      ↯ publish! 'A' ⟶ 1 ；
      
      • 'B' - 2 ∶ Boolᵍ ≔ input "Enter B's choice." ；
      ↯ publish! 'B' ⟶ 2 ；
      
      if  𝓵 1 == 𝓵 2  
        then (↯ withdraw! 'A' ⟵ 2 ₗ ；₁)
        else (↯ withdraw! 'B' ⟵ 2 ₗ ；₁)
  ⁆

    ∷ []

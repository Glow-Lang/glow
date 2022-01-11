
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


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool renaming ()

open import Cubical.Data.Nat.Order.Recursive
open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

-- I am intentionaly avoiding "statement" and "expression" nomenclature to avodi confusion with previous layers
-- In the future we can intorduce it, or maybe on next level



-- infixr 80 •

-- pattern • x = just x

-- TODO : check
-- can you define private function?
---- can you publish function?
---- can you definie interaction within interaction ?


data GType : Type₀ where
  Boolᵍ : GType 
  Intᵍ : GType
  Natᵍ : GType
  Unitᵍ : GType

-- GTy≟ : Discrete GType
-- GTy≟ x y = {!x y!}

GTy== : GType → GType → Bool
GTy== Boolᵍ Boolᵍ = true
GTy== Intᵍ Intᵍ = true
GTy== Natᵍ Natᵍ = true
GTy== Unitᵍ Unitᵍ = true
GTy== _ _ = false


GTypeAgdaRep : GType → Type₀
GTypeAgdaRep Boolᵍ = Bool
GTypeAgdaRep Intᵍ = ℤ
GTypeAgdaRep Natᵍ = ℕ
GTypeAgdaRep Unitᵍ = Unit

record IsGlowTy (A : Type₀) : Type₁ where
  field
    glowRep : GType
    glowRep-coh : A ≡ GTypeAgdaRep glowRep
    cast : A → GTypeAgdaRep glowRep

instance
  Bool-IsGlowTy : IsGlowTy Bool
  Bool-IsGlowTy = record { glowRep = Boolᵍ ; glowRep-coh = refl ; cast = idfun _}

instance
  ℤ-IsGlowTy : IsGlowTy ℤ
  ℤ-IsGlowTy = record { glowRep = Intᵍ  ; glowRep-coh = refl ; cast = idfun _ }

instance
  ℕ-IsGlowTy : IsGlowTy ℕ
  ℕ-IsGlowTy = record { glowRep = Natᵍ  ; glowRep-coh = refl ; cast = idfun _ }

instance
  Unit-IsGlowTy : IsGlowTy Unit
  Unit-IsGlowTy = record { glowRep = Unitᵍ  ; glowRep-coh = refl ; cast = idfun _ }


IdentifierTy = String

IdentifierTyTest : IdentifierTy → IdentifierTy → Bool 
IdentifierTyTest = primStringEquality

Bool→Type' : Bool → hProp ℓ-zero
Bool→Type' true = ⊤
Bool→Type' false = ⊥

Dec-Bool→Type' : ∀ {b} → ⟨ Decₚ (Bool→Type' b) ⟩
Dec-Bool→Type' {false} = no (idfun _)
Dec-Bool→Type' {true} = yes _


True' : ∀ {a} {A : Type a} → Dec A → hProp ℓ-zero
True' Q = Bool→Type' (Dec→Bool Q)


False' : ∀ {a} {A : Type a} → Dec A → hProp ℓ-zero
False' Q = Bool→Type' (not (Dec→Bool Q))



MemberByTest : ∀ {ℓ} → {A : Type ℓ} → (A → A → Bool) → List A → A → Bool 
MemberByTest f [] _ = false
MemberByTest f (x' ∷ xs) x =
  if f x x'
   then  true
   else MemberByTest f xs x

MemberBy : ∀ {ℓ} → {A : Type ℓ} → (A → A → Bool) → List A → A → hProp ℓ-zero 
MemberBy f l x = (Bool→Type' (MemberByTest f l x))


findBy : ∀ {ℓ} → {A : Type ℓ} → (A → Bool) → List A → Maybe A
findBy _ [] = nothing
findBy test (x ∷ xs) = if (test x) then (just x) else (findBy test xs)

record IdentifierWithType : Type₀ where
  pattern
  constructor iwt
  field
    name : String
    type : GType



record InteractionHead : Type₀ where
  constructor interactionHead
  pattern
  field
    participants : List IdentifierTy
    parameters : List IdentifierWithType



  IsParticipantId : IdentifierTy → hProp ℓ-zero
  IsParticipantId name =
      MemberBy IdentifierTyTest participants name


  record ParticipantId : Type₀ where
    constructor pId
    field
      name : IdentifierTy
      {isIn} : ⟨ IsParticipantId name ⟩

  open ParticipantId


  Scope : Type₀
  Scope = Maybe ParticipantId


  record ContextEntry : Type₀ where
    constructor ice

    field
      scope : Scope
      name : IdentifierTy
      type : GType

  open ContextEntry

  record Context : Type₀ where
    pattern
    constructor con


    field
      entries : List ContextEntry

    field
      scope' : Scope



    _canAccessTest_ : Scope → Scope → Bool
    _ canAccessTest nothing = true
    just x canAccessTest just x₁ = IdentifierTyTest (name x) (name x₁)
    nothing canAccessTest just x₁ = false

    _canAccess_ : Scope → Scope → hProp ℓ-zero
    x canAccess x₁ = Bool→Type' (x canAccessTest x₁)


    entryLookup : IdentifierTy → Maybe ContextEntry
    entryLookup x = findBy (IdentifierTyTest x ∘ name) entries

    IsDefinedSymbolOfTyTest : GType → IdentifierTy → Bool
    IsDefinedSymbolOfTyTest ty x =
      recMaybe false (λ y → (scope') canAccessTest (scope y)) (entryLookup x) 

    IsDefinedSymbolOfTy : GType → IdentifierTy → hProp ℓ-zero
    IsDefinedSymbolOfTy ty x = Bool→Type' (IsDefinedSymbolOfTyTest ty x) 

    record DefinedSymbolOfTy (Τ : GType) : Type ℓ-zero where
      constructor dsot
      field
        name : IdentifierTy
        {isDefinedSymbolOfTy} : ⟨ IsDefinedSymbolOfTy Τ name ⟩

    IsPrivateSymbolOfTyTest : ParticipantId → GType → IdentifierTy → Bool
    IsPrivateSymbolOfTyTest p ty x =
      recMaybe
         false
         (λ y → recMaybe false (λ p' → IdentifierTyTest (name p) (name p')) (scope y))
         (entryLookup x)

    IsPrivateSymbolOfTy : ParticipantId → GType → IdentifierTy → hProp ℓ-zero
    IsPrivateSymbolOfTy p ty x = Bool→Type' (IsDefinedSymbolOfTyTest ty x) 

    record PrivateSymbolOf (p : ParticipantId) : Type ℓ-zero where
      constructor psof
      field
        type : GType
        name : IdentifierTy
        {isDefinedSymbolOfTy} : ⟨ IsPrivateSymbolOfTy p type name ⟩

    AllowedScopeNarrowingTest : Scope → Bool
    AllowedScopeNarrowingTest nothing = true
    AllowedScopeNarrowingTest (just x) = caseMaybe true false scope'

    AllowedScopeNarrowing : Scope → hProp ℓ-zero
    AllowedScopeNarrowing =  Bool→Type' ∘ AllowedScopeNarrowingTest  

    IsConsensus : hProp ℓ-zero
    IsConsensus = caseMaybe ⊤ ⊥ scope'


  open Context public

  emptyContext : Context
  emptyContext = con [] nothing


  narrow : (Γ : Context) → (s : Scope)  → (⟨ AllowedScopeNarrowing Γ s ⟩) → Context
  narrow (con y nothing) a _ = (con y a)
  narrow a@(con _ (just x)) _ _ = a




  data Stmnt (Γ : Context) : Type₀

  data BStmnt (Γ : Context) : Type₀


  data NBStmnt (Γ : Context) : Type₀

  data NBStmnt+Expr (Γ : Context) : Type₀

  data Expr (Γ : Context) (Τ : GType): Type₀

  bindingMechanics : (Γ : Context) → BStmnt Γ → Context 

  bindingMechanics' : (Γ : Context) → Stmnt Γ → Context 


  record Body (Γ : _) (Τ : _ ) : Type₀ where
    pattern
    inductive
    constructor bodyR
    field
      stmnts : Linked bindingMechanics' Γ
      expr : Expr (foldLinked stmnts) Τ


  record Module : Type₀ where
    pattern
    constructor moduleᵍ
    field
      stmnts : Linked bindingMechanics' emptyContext




  data Expr Γ Τ where
    var : DefinedSymbolOfTy Γ Τ → Expr Γ Τ
    body : Body Γ Τ → Expr Γ Τ
    lit : GTypeAgdaRep Τ → Expr Γ Τ

  data Stmnt Γ where
    -- not necessary binding, but rather context changing
    bindingS : BStmnt Γ → Stmnt Γ
    nonBindingS : NBStmnt+Expr Γ → Stmnt Γ

  data BStmnt Γ where
                  -- warning: scope in "ce" is interpreted in unusual way!
                  -- (TODO : consider speical type here)
    BS-let : (ce : ContextEntry) → {asn : ⟨ AllowedScopeNarrowing Γ (scope ce) ⟩}
                → Expr (narrow Γ (scope ce) asn) (type ce) → BStmnt Γ    
    BS-publish!_⟶_ : (p : ParticipantId) → List (PrivateSymbolOf Γ p) → BStmnt Γ
    -- verify! ‹ids›

  data NBStmnt Γ where
    NBS-require! : Expr Γ Boolᵍ → NBStmnt Γ
    NBS-deposit!_ ⟶_ : ParticipantId → Expr Γ Natᵍ → NBStmnt Γ
    NBS-withdraw!_ ⟵_ : ParticipantId → Expr Γ Natᵍ → NBStmnt Γ


  data NBStmnt+Expr Γ where
    stmntNBS : NBStmnt Γ → {⟨ IsConsensus Γ ⟩} → NBStmnt+Expr Γ
    exprNBS : ∀ {Τ} → Expr Γ Τ → NBStmnt+Expr Γ

  bindingMechanics Γ (BS-let ce _) = record Γ { entries =  ce ∷ entries Γ }
  bindingMechanics Γ (BS-publish! p ⟶ x) =
    let makePublic  e = if (recMaybe false (λ p' → IdentifierTyTest (name p) (name p))
                                     (scope e))
                            then (record e { scope = nothing })
                            else e

    in record Γ { entries =  Cubical.Data.List.map makePublic (entries Γ) }


  bindingMechanics' Γ (bindingS x) = bindingMechanics Γ x
  bindingMechanics' Γ (nonBindingS x) = Γ

record Interaction : Type₀ where
  pattern
  constructor interaction
  field
    head : InteractionHead
    
  open InteractionHead head public

  field
    code : Linked bindingMechanics' emptyContext 



infixl 6 interaction⟨_,_⟩_
infixr 50 _∶_ 

infixl 15 _;_
infix 17 _;₁
infixl 15 _;'_

infix 30 set_∶_≔_


infix 60 <_>

pattern interaction⟨_,_⟩ prts prms stmnts = interaction (interactionHead prts prms ) stmnts

pattern _∶_ x y = iwt x y 

pattern _;_ x y = _L∷_ x y


pattern _;₁ x = L[] ; x  

pattern _;'_ x y = x ;₁ ; y  

pattern set_∶_≔_ x y z =
  InteractionHead.bindingS (InteractionHead.BS-let (InteractionHead.ice nothing x y) z)

pattern at_set_∶_≔_ p x y z =
  InteractionHead.bindingS
     (InteractionHead.BS-let (InteractionHead.ice (just (InteractionHead.pId p)) x y) z)

<_> : ∀ {IH Γ} → {A : Type₀} → ⦃ isGlowTy : IsGlowTy A ⦄ →
         A →  InteractionHead.Expr IH Γ (IsGlowTy.glowRep isGlowTy)
<_> {IH} {Γ} {A} ⦃ isGlowTy ⦄ x = InteractionHead.lit (IsGlowTy.cast isGlowTy x)

infixr 60 v_

pattern v_ x = InteractionHead.var (InteractionHead.dsot x)


someInteraction : Interaction
someInteraction =
   interaction⟨   "A" ∷ "B" ∷ [] , "b1" ∶ Boolᵍ ∷ [] ⟩ (
        set "x" ∶ Boolᵍ ≔ < true > ;'
        at "B" set "y" ∶ Boolᵍ ≔ < true > ;
        set "xx" ∶ Boolᵍ ≔ v "x" ;
        at "B" set "yy" ∶ Natᵍ ≔ v "y" ;
        {!!}
     )








--   -- --     -- recMaybe ⟨ ⊥ ⟩ (λ x → if (GTy== ty x) then ⟨ ⊤ ⟩ else' ⟨ ⊥ ⟩) (getSymbolTy sc vI)


--   -- --     -- IsFreeSymbol : String → Type₀
--   -- --     -- IsFreeSymbol s = ? 

--   -- -- --     getSymbolTy : Scope iH → ValIdentifier → Maybe GType
--   -- -- --     getSymbolTy sc = {!!}

--   -- -- --     IsPrivateSymbolOf : ValidParticipantSymbol iH → ValIdentifier → Type₀
--   -- -- --     IsPrivateSymbolOf = {!!}


--   -- -- --   open Context

--   -- -- --   emptyContext : ∀ iH → Context iH
--   -- -- --   emptyContext iH = iCon [] ◦

--   -- -- --   addToContext : ∀ {iH} → Context iH → SymInContext iH →  Context iH
--   -- -- --   definedS (addToContext {iH} x x₁) = x₁ ∷ x .definedS
--   -- -- --   scope (addToContext {iH} x x₁) = x .scope

--   -- -- --   data TopLevelDefinition : Type₀

--   -- -- --   Module = List TopLevelDefinition


--   -- -- --   data IBody {iH : IHead} (iC₀ : Context iH) : Type₀

--   -- -- --   contextAfter : {iH : IHead} {iC₀ : Context iH} → IBody iC₀ → Context iH

--   -- -- --   data IEffect {iH : IHead} {iC₀ : Context iH}
--   -- -- --                             (ib : IBody iC₀) : Type₀

--   -- -- --   data IPart {iH : IHead} {iC₀ : Context iH}
--   -- -- --                             (ib : IBody iC₀) : Type₀

--   -- --  --   data IValue {iH : IHead} {iC₀ : Context iH}
--   -- -- --                             (ib : IBody iC₀) (sC : Scope iH) GType : Type₀




--   -- -- --   Disc→Ty : ∀ {ℓ} → ∀ {A : Type ℓ} → Dec A → Type₀
--   -- -- --   Disc→Ty (yes p) = ⟨ ⊤ ⟩
--   -- -- --   Disc→Ty (no ¬p) = ⟨ ⊥ ⟩

--   -- -- --   =ℕTy : ℕ → ℕ → Type₀
--   -- -- --   =ℕTy x x₁ = Disc→Ty (discreteℕ x x₁)


--   -- -- --   -- infixl 6 interaction_⟨_,_⟩⁅_⁆

--   -- -- --   -- infix 60 _ₗ
--   -- -- --   -- infix 60 _ₗ'
--   -- -- --   -- infix 60 𝓁_

--   -- -- --   record IDefinition : Type₀ where
--   -- -- --     constructor iDefinition
--   -- -- --     field
--   -- -- --       name : Identifier
--   -- -- --       head : IHead
--   -- -- --       body : IBody (emptyContext head)

--   -- -- --   data TopLevelDefinition where
--   -- -- --     tlInteraction : IDefinition → TopLevelDefinition


--   -- -- --   data IEffect {iH} ib where
--   -- -- --     -- verify!  : {!!} → IEffect ib
--   -- -- --     publish!_⟶𝓁_ : (pC : ValidParticipantSymbol iH) → 
--   -- -- --                       (vI : ValIdentifier) → {_ :  IsPrivateSymbolOf (contextAfter ib) pC vI } → IEffect ib
--   -- -- --     deposit!_⟶_  : (pC : ValidParticipantSymbol iH)
--   -- -- --                      → IValue ib ◦ Intᵍ
--   -- -- --                      → IEffect ib
--   -- -- --     withdraw!_⟵_ : (pC : ValidParticipantSymbol iH)
--   -- -- --                      → IValue ib ◦ Intᵍ
--   -- -- --                      → IEffect ib

--   -- -- --   infix 40 _[𝓁_∶_]≔_
--   -- -- --   infix 20 ↯_

--   -- -- --   data IPart {iH} {iC₀} ib where
--   -- -- --     _[𝓁_∶_]≔_ : (j : Scope iH) 
--   -- -- --                  (k : ValIdentifier) → (gTy : GType) →  IValue ib {!!} gTy
--   -- -- --                    → IPart ib
--   -- -- --     ↯_ : IEffect ib → IPart ib
--   -- -- --     if_then_else_ : IValue ib {!!} Boolᵍ →
--   -- -- --                       IBody (contextAfter ib) →
--   -- -- --                       IBody (contextAfter ib) → IPart ib


--   -- -- --   -- record IsGlowTy {a} (A : Type a) : Type a where
--   -- -- --   --   field
--   -- -- --   --     glowRep : GType
--   -- -- --   --     cast : A → GTypeAgdaRep glowRep

--   -- -- --   -- instance
--   -- -- --   --   Bool-IsGlowTy : IsGlowTy Bool
--   -- -- --   --   Bool-IsGlowTy = record { glowRep = Boolᵍ ; cast = idfun _}

--   -- -- --   -- instance
--   -- -- --   --   ℤ-IsGlowTy : IsGlowTy ℤ
--   -- -- --   --   ℤ-IsGlowTy = record { glowRep = Intᵍ  ; cast = idfun _ }

--   -- -- --   -- instance
--   -- -- --   --   ℕ-IsGlowTy : IsGlowTy ℕ
--   -- -- --   --   ℕ-IsGlowTy = record { glowRep = Natᵍ  ; cast = idfun _ }


--   -- -- --   -- infix 50 _==_

--   -- -- --   data IValue {iH} {iC₀} ib sC gTy where
--   -- -- --     𝓁_ : (k : ValIdentifier) → {kProof : IsDefinedSymbolOfTy iC₀ sC k gTy} → IValue ib sC gTy
--   -- -- --     _ₗ' : GTypeAgdaRep gTy → IValue ib sC gTy
--   -- -- --     input : String → IValue ib sC gTy
--   -- -- --     _==_ : IValue ib sC gTy → IValue ib sC  gTy → IValue ib sC  gTy
--   -- -- --     -- 𝓹 : (k : ℕ) → {_ : isParticipantSymbol ib k} →  IValue ib gTy

--   -- -- --   -- _ₗ : {participants : List Char} {paramtersTy : List IParameter} → 
--   -- -- --   --       {ib : IBody {participants} {paramtersTy}} → 
--   -- -- --   --                         {A : Type₀} → {{isGlowTy : IsGlowTy A}} →
--   -- -- --   --                         A →  IValue ib (IsGlowTy.glowRep isGlowTy)
--   -- -- --   -- _ₗ {participants} {paramtersTy} {ib} {A} ⦃ isGlowTy ⦄ x = IsGlowTy.cast isGlowTy x ₗ' 




--   -- -- --   -- infixl 15 _；_
--   -- -- --   -- infix 17 _；₁
--   -- -- --   -- infixl 15 _；'_

--   -- -- --   data Statements {iH} iC₀ where
--   -- -- --    ∙ib : Body iC₀
--   -- -- --    _；_ : ∀ {iC} → (ss : Body {iH} iC)
--   -- -- --            →  (s : IPart ss) → Body iC₀

--   -- -- --   -- pattern _；₁ x = ∙ib ； x  

--   -- -- --   -- pattern _；'_ x y = ∙ib ； x ； y  

--   -- -- --   contextAfter = {!!}

--   -- -- --   -- contextAfter {iC₀ = iC₀} ∙ib = iC₀
--   -- -- --   -- contextAfter (x ； j [𝓁 k ∶ gTy ]≔  x₁) =
--   -- -- --   --   addToContext (contextAfter x) (symInContext j k gTy)
--   -- -- --   -- contextAfter (x ； _) = contextAfter x



--   -- -- --   -- getFreeSymbol = ?
--   -- -- --   -- -- getFreeSymbol {paramtersTy = l} ∙ib = length l
--   -- -- --   -- -- getFreeSymbol (x ； (_ [𝓁 _ ∶ _ ]≔ _)) = suc (getFreeSymbol x)
--   -- -- --   -- -- getFreeSymbol (x ； _) = getFreeSymbol x

--   -- -- --   -- getSymbolTy ∙ib k = ◦
--   -- -- --   -- getSymbolTy (ib ； _ [𝓁 k' ∶ gTy ]≔ x) k with discreteℕ k k'
--   -- -- --   -- ... | yes p = • gTy
--   -- -- --   -- ... | no ¬p = getSymbolTy ib k
--   -- -- --   -- getSymbolTy (ib ； _) k = getSymbolTy ib k

--   -- -- --   -- -- -- testModule : Module
--   -- -- --   -- -- -- testModule =
--   -- -- --   -- -- --   interaction⟨ 2 , Boolᵍ ∷ [] ⟩⁅
--   -- -- --   -- -- --      ∙ib ；
--   -- -- --   -- -- --      ◦ - 1 ∶ Boolᵍ ≔ false ₗ ；
--   -- -- --   -- -- --      ◦ - 1 ∶ Natᵍ ≔ 3 ₗ ；

--   -- -- --   -- -- --      {! !} ；
--   -- -- --   -- -- --      {!!}
--   -- -- --   -- -- --   ⁆

--   -- -- --   -- -- --     ∷ []


--   -- -- --   -- -- boolGameModule : Module
--   -- -- --   -- -- boolGameModule =
--   -- -- --   -- --   interaction "boolGame" ⟨  'A' ∷ 'B' ∷ [] , [ "p" ∶ Intᵍ ] ∷ [] ⟩⁅ ∙ib ； 

--   -- -- --   -- --       ↯ deposit! 'A' ⟶ 1 ₗ ； 
--   -- -- --   -- --       ↯ deposit! 'B' ⟶ 1 ₗ ；

--   -- -- --   -- --       • 'A' [𝓁 1 ∶ Boolᵍ ]≔ input "Enter A's choice." ；
--   -- -- --   -- --       ↯ publish! 'A' ⟶𝓁 1 ；

--   -- -- --   -- --       • 'B' [𝓁 2 ∶ Boolᵍ ]≔ input "Enter B's choice." ；
--   -- -- --   -- --       ↯ publish! 'B' ⟶𝓁 2 ；

--   -- -- --   -- --       ◦ [𝓁 3 ∶ Intᵍ ]≔  1 ₗ  ；

--   -- -- --   -- --       if  𝓁 1 == 𝓁 2   
--   -- -- --   -- --         then (↯ withdraw! 'A' ⟵ 2 ₗ ；₁)
--   -- -- --   -- --         else (↯ withdraw! 'B' ⟵ 2 ₗ ；₁)
--   -- -- --   -- --   ⁆

--   -- -- --   -- --     ∷ []

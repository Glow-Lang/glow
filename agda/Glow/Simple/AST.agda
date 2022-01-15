
{-# OPTIONS --cubical  #-}
module Glow.Simple.AST where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

-- I am intentionaly avoiding "statement" and "expression" nomenclature to avodi confusion with previous layers
-- In the future we can intorduce it, or maybe on next level


isProp-lemma : ∀ {ℓ} → {A B : hProp ℓ} → (a : ⟨ A ⟩ ) → (b : ⟨ B ⟩)
                    →  ⟨ A ⟩ ≡ ⟨ B ⟩
isProp-lemma {A = A} {B} a b = isoToPath (iso (λ _ → b) (λ _ → a) (λ _ → snd B _ _) (λ _ → snd A _ _))

isProp-lemma' :  ∀ {ℓ} → {A B : hProp ℓ} → (a : ⟨ A ⟩ ) → (b : ⟨ B ⟩)
                    → PathP (λ x → ((isProp-lemma {A = A} {B} a b) ∙ refl) x ) a b
isProp-lemma' {A = A} {B = B} a b = compPathP (transport-filler (isProp-lemma {A = A} {B} a b) a ) (snd B _ _)

and-comm  : ∀ x y → x and y ≡ y and x
and-comm false false = refl
and-comm false true = refl
and-comm true false = refl
and-comm true true = refl

and-F  : ∀ x → x and false ≡ false
and-F false = refl
and-F true = refl

F-and  : ∀ x → false and x ≡ false
F-and false = refl
F-and true = refl

and-T  : ∀ x → x and true ≡ x
and-T false = refl
and-T true = refl

T-and  : ∀ x → true and x ≡ x
T-and false = refl
T-and true = refl

and-identityˡ : ∀ x → true and x ≡ x
and-identityˡ false = refl
and-identityˡ true  = refl

and-identityʳ : ∀ x → x and true ≡ x
and-identityʳ false = refl
and-identityʳ true  = refl


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


data GType : Type₀ where
  Bool : GType 
  Int : GType
  Nat : GType
  Unitᵍ : GType

-- GTy≟ : Discrete GType
-- GTy≟ x y = {!x y!}

GTy== : GType → GType → 𝟚
GTy== Bool Bool = true
GTy== Int Int = true
GTy== Nat Nat = true
GTy== Unitᵍ Unitᵍ = true
GTy== _ _ = false


GTypeAgdaRep : GType → Type₀
GTypeAgdaRep Bool = 𝟚
GTypeAgdaRep Int = ℤ
GTypeAgdaRep Nat = ℕ
GTypeAgdaRep Unitᵍ = Unit

record IsGlowTy (A : Type₀) : Type₁ where
  field
    glowRep : GType
    glowRep-coh : A ≡ GTypeAgdaRep glowRep
    cast : A → GTypeAgdaRep glowRep

instance
  Bool-IsGlowTy : IsGlowTy 𝟚
  Bool-IsGlowTy = record { glowRep = Bool ; glowRep-coh = refl ; cast = idfun _}

instance
  ℤ-IsGlowTy : IsGlowTy ℤ
  ℤ-IsGlowTy = record { glowRep = Int  ; glowRep-coh = refl ; cast = idfun _ }

instance
  ℕ-IsGlowTy : IsGlowTy ℕ
  ℕ-IsGlowTy = record { glowRep = Nat  ; glowRep-coh = refl ; cast = idfun _ }

instance
  Unit-IsGlowTy : IsGlowTy Unit
  Unit-IsGlowTy = record { glowRep = Unitᵍ  ; glowRep-coh = refl ; cast = idfun _ }



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

IdentifierTy = String

IdentifierTyTest : IdentifierTy → IdentifierTy → 𝟚 
IdentifierTyTest = primStringEquality



Bool→Type' : 𝟚 → hProp ℓ-zero
Bool→Type' true = ⊤
Bool→Type' false = ⊥

Bool→Type'-≡ : ∀ {x} → ⟨ Bool→Type' x ⟩ ≡ (x ≡ true) 
Bool→Type'-≡ {false} = isoToPath (iso empty-elim false≢true (λ b → isSetBool _ _ _ _) empty-elim)
Bool→Type'-≡ {true} = isoToPath (iso (λ x → refl) (λ _ → _) (λ b → isSetBool _ _ _ _) λ a i → _)

¬-Bool→Type'-≡ : ∀ {x} → ⟨ ¬ Bool→Type' x ⟩ ≡ (x ≡ false) 
¬-Bool→Type'-≡ {false} = isoToPath (iso (λ x → refl) (λ x → λ x₁ → x₁) (λ _ → isSetBool _ _ _ _) λ a → isPropΠ (const isProp⊥) _ _)
¬-Bool→Type'-≡ {true} = isoToPath (iso (λ x → empty-elim (x _)) (λ x _ → true≢false x ) (λ _ → isSetBool _ _ _ _) λ a → isPropΠ (const isProp⊥) _ _)

-- Bool→Type'-witness : ∀ x → 
-- Bool→Type'-witness = ?

Dec-Bool→Type' : ∀ {b} → ⟨ Decₚ (Bool→Type' b) ⟩
Dec-Bool→Type' {false} = no (idfun _)
Dec-Bool→Type' {true} = yes _

𝟚-byEq : ∀ {x y} → x ≡ y → ⟨ Bool→Type' x ⟩ → ⟨ Bool→Type' y ⟩ 
𝟚-byEq = transport ∘ (cong (fst ∘  Bool→Type'))

GTy==-≡ : ∀ {Τ₁ Τ₂} → ⟨ Bool→Type' (GTy== Τ₁ Τ₂) ⟩ → Τ₁ ≡ Τ₂  
GTy==-≡ {Bool} {Bool} _ = refl
GTy==-≡ {Int} {Int} _ = refl
GTy==-≡ {Nat} {Nat} _ = refl
GTy==-≡ {Unitᵍ} {Unitᵍ} _ = refl

and-True' : ∀ {x y} → ⟨ Bool→Type' (x and y) ⟩ → ⟨ Bool→Type' x ⟩ × ⟨ Bool→Type' y ⟩ 
and-True' {false} {false} ()
and-True' {false} {true} ()
and-True' {true} {false} ()
and-True' {true} {true} (lift _) = _ , _

True' : ∀ {a} {A : Type a} → Dec A → hProp ℓ-zero
True' Q = Bool→Type' (Dec→Bool Q)

False' : ∀ {a} {A : Type a} → Dec A → hProp ℓ-zero
False' Q = Bool→Type' (not (Dec→Bool Q))

𝟚-elim : ∀ {a} {A : 𝟚 → Type a} → A false → A true → ∀ b → A b
𝟚-elim {a} {A} x x₁ false = x
𝟚-elim {a} {A} x x₁ true = x₁

maybe-elim : ∀ {a} {A : Type a} {B : Maybe A  → Type a} → B nothing → (∀ a → B (just a)) → ∀ x → B x
maybe-elim x x₁ nothing = x
maybe-elim x x₁ (just x₂) = x₁ x₂

MemberByTest : ∀ {ℓ} → {A : Type ℓ} → (A → A → 𝟚) → List A → A → 𝟚 
MemberByTest f [] _ = false
MemberByTest f (x' ∷ xs) x =
  f x' x or MemberByTest f xs x

MemberBy : ∀ {ℓ} → {A : Type ℓ} → (A → A → 𝟚) → List A → A → hProp ℓ-zero 
MemberBy f l x = (Bool→Type' (MemberByTest f l x))

UniqueByTest : ∀ {ℓ} → {A : Type ℓ} → (A → A → 𝟚) → List A → 𝟚 
UniqueByTest f [] = true
UniqueByTest f (x ∷ xs) = ( not (MemberByTest f xs x) and UniqueByTest f xs )


UniqueBy : ∀ {ℓ} → {A : Type ℓ} → (A → A → 𝟚) → List A → hProp ℓ-zero 
UniqueBy f x = Bool→Type' ( UniqueByTest f x )


findBy : ∀ {ℓ} → {A : Type ℓ} → (A → 𝟚) → List A → Maybe A
findBy _ [] = nothing
findBy test (x ∷ xs) = if (test x) then (just x) else (findBy test xs)

 

record IdentifierWithType : Type₀ where
  pattern
  constructor iwt
  field
    name : String
    type : GType

open IdentifierWithType

ParametersValue : List IdentifierWithType →  Type₀
ParametersValue [] = Unit
ParametersValue (x ∷ xs) = GTypeAgdaRep (type x) × ParametersValue xs

lookup-ParametersValue : (l : List IdentifierWithType) → ParametersValue l
                           → (x : IdentifierWithType)
                           → ⟨ MemberBy (λ x₁ x₂ → IdentifierTyTest (name x₁) (name x₂)
                                                     and GTy== (type x₁) (type x₂)) l x ⟩ 
                           → GTypeAgdaRep (type x)
lookup-ParametersValue (x ∷ l) (v , vv) x₃ x₂ with Dec-Bool→Type' {(IdentifierTyTest (name x) (name x₃) and GTy== (type x) (type x₃))}
... | yes p = subst GTypeAgdaRep (GTy==-≡ (proj₂ (and-True' {IdentifierTyTest (name x) (name x₃)} {GTy== (type x) (type x₃)} p))) v  
... | no ¬p =  let zz = subst {x = (IdentifierTyTest (name x) (name x₃) and GTy== (type x) (type x₃))} (λ bb → fst
                            (Bool→Type'
                             (bb
                              or
                              MemberByTest
                              (λ x₁ x₄ →
                                 IdentifierTyTest (name x₁) (name x₄) and
                                 GTy== (type x₁) (type x₄))
                              l x₃))) (transport (¬-Bool→Type'-≡) ¬p ) x₂
               in lookup-ParametersValue (l) (vv) x₃ (𝟚-byEq  (or-identityˡ (MemberByTest
                              (λ x₁ x₄ →
                                 IdentifierTyTest (name x₁) (name x₄) and
                                 GTy== (type x₁) (type x₄))
                              l x₃)) zz)  

record InteractionHead : Type₀ where
  constructor interactionHead
  -- inductive
  pattern
  field
    participants : List IdentifierTy
    parameters : List IdentifierWithType
    {uniqueParams} : ⟨ UniqueBy (λ x y → IdentifierTyTest (name x) (name y)) parameters  ⟩


  IsParticipantId : IdentifierTy → hProp ℓ-zero
  IsParticipantId name =
      MemberBy IdentifierTyTest participants name

  paramLookup : IdentifierTy → Maybe IdentifierWithType
  paramLookup x = findBy (IdentifierTyTest x ∘ name) parameters


  record ParticipantId : Type₀ where
    constructor pId
    field
      name : IdentifierTy
      {isIn} : ⟨ IsParticipantId name ⟩

  open ParticipantId public


  Scope : Type₀
  Scope = Maybe ParticipantId


  record ContextEntry : Type₀ where
    constructor ice

    field
      scope : Scope
      name : IdentifierTy
      type : GType


  open ContextEntry public

  AType : ContextEntry → Type₀
  AType ce = GTypeAgdaRep (ce .type)



  record Context : Type₀ where
    pattern
    constructor con


    field
      entries : List ContextEntry

    field
      scope' : Scope



    _canAccessTest_ : Scope → Scope → 𝟚
    _ canAccessTest nothing = true
    just x canAccessTest just x₁ = IdentifierTyTest (name x) (name x₁)
    nothing canAccessTest just x₁ = false

    _canAccess_ : Scope → Scope → hProp ℓ-zero
    x canAccess x₁ = Bool→Type' (x canAccessTest x₁)


    entryLookup : IdentifierTy → Maybe ContextEntry
    entryLookup x = findBy (IdentifierTyTest x ∘ name) entries


    IsMbShadowedParamOfTyTest : GType → IdentifierTy → 𝟚
    IsMbShadowedParamOfTyTest ty x =
      recMaybe false 
            (λ y → GTy== (type y) ty)
              (paramLookup x)


    IsDefinedVariableOfTyTest : GType → IdentifierTy → 𝟚
    IsDefinedVariableOfTyTest ty x =
      recMaybe false
          (λ y → (scope') canAccessTest (scope y)
                    and  GTy== (type y) ty)
         (entryLookup x) 


    IsDefinedSymbolOfTyTest : GType → IdentifierTy → 𝟚
    IsDefinedSymbolOfTyTest ty x =
      let inParams = IsMbShadowedParamOfTyTest ty x

      in recMaybe inParams
          (λ y → (scope') canAccessTest (scope y)
                    and  GTy== (type y) ty)
         (entryLookup x) 

    IsDefinedSymbolOfTy : GType → IdentifierTy → hProp ℓ-zero
    IsDefinedSymbolOfTy ty x = Bool→Type' (IsDefinedSymbolOfTyTest ty x) 

    record DefinedSymbolOfTy (Τ : GType) : Type ℓ-zero where
      constructor dsot
      field
        name : IdentifierTy
        {isDefinedSymbolOfTy} : ⟨ IsDefinedSymbolOfTy Τ name ⟩

    open DefinedSymbolOfTy public

    DefinedSymbolOfTy-Cases : GType → IdentifierTy →  Type₀
    DefinedSymbolOfTy-Cases ty x =
      ⟨ Bool→Type' (IsDefinedVariableOfTyTest ty x) ⟩
        ⊎
      ⟨ Bool→Type' (caseMaybe true false (entryLookup x)  and (IsMbShadowedParamOfTyTest ty x) ) ⟩


    DefinedSymbolOfTy-case : ∀ {Τ} → (x : IdentifierTy) →
      (⟨ IsDefinedSymbolOfTy Τ x ⟩ → DefinedSymbolOfTy-Cases Τ x)
        × (DefinedSymbolOfTy-Cases Τ x → ⟨ IsDefinedSymbolOfTy Τ x ⟩) 
                                                     
    DefinedSymbolOfTy-case {Τ} x = 
     let mb0 = (entryLookup (x))
         b0 : ContextEntry → 𝟚
         b0 = (λ y → (scope' canAccessTest scope y) and GTy== (type y) Τ)
         mb1 = (paramLookup (x))
         b1 : IdentifierWithType → 𝟚
         b1 = (λ y → GTy== (type y) Τ)
         elimTy :  Maybe ContextEntry → (ContextEntry → 𝟚) →   Maybe IdentifierWithType →  (IdentifierWithType → 𝟚) →  Type₀
         elimTy mb0  b0 mb1  b1  =
              let T₁ = ⟨ Bool→Type' (recMaybe (recMaybe false b1 mb1) b0 mb0 ) ⟩
                  T₂ = (⟨ Bool→Type' (recMaybe false b0 mb0) ⟩ ⊎ ⟨ Bool→Type' ((caseMaybe true false mb0) and recMaybe false b1 mb1) ⟩ )
              in (T₁ → T₂) × (T₂ → T₁)

         z : elimTy mb0  b0 mb1  b1
         z =  maybe-elim {B = λ mb0 → elimTy mb0 b0 mb1 b1  }
              (((_⊎_.inr ∘ transport (cong {x = recMaybe false b1 mb1} (fst ∘ Bool→Type') (sym (T-and _)))))
               , Cubical.Data.Sum.rec (λ ()) (transport (cong {y = recMaybe false b1 mb1} (fst ∘ Bool→Type') ((T-and _)))))
              (λ a → _⊎_.inl ,
                 Cubical.Data.Sum.rec (idfun _)
                λ x₁ → empty-elim (proj₁ (and-True' {false} {recMaybe false (λ y → GTy== (type y) Τ) (paramLookup x)} x₁))
                   )
              mb0
     in z

    IsPrivateSymbolOfTest : ParticipantId → IdentifierTy → 𝟚
    IsPrivateSymbolOfTest p x =
      recMaybe
         false
         (λ y → recMaybe false (λ p' → IdentifierTyTest (name p) (name p')) (scope y))
         (entryLookup x)

    IsPrivateSymbolOf : ParticipantId → IdentifierTy → hProp ℓ-zero
    IsPrivateSymbolOf p x = Bool→Type' (IsPrivateSymbolOfTest p x) 


    record PrivateSymbolOf (p : ParticipantId) : Type ℓ-zero where
      pattern
      constructor psof
      field
        name : IdentifierTy
        {isDefinedSymbolOf} : ⟨ IsPrivateSymbolOf p name ⟩

    open PrivateSymbolOf public

    IsConsensus : hProp ℓ-zero
    IsConsensus = caseMaybe ⊤ ⊥ scope'


  open Context public

  emptyContext : Context
  emptyContext = con [] nothing

  prependContext : ContextEntry → Context →  Context
  prependContext x Γ = record Γ { entries =   Γ .entries ∷ʳ x } 

  addToContext : Context → ContextEntry → Context
  addToContext Γ x = record Γ { entries =  x ∷ Γ .entries } 

  popFromCtxt : Context → Maybe ContextEntry × Context
  popFromCtxt Γ = (pop (Γ .entries)) , record Γ { entries = tail (Γ .entries) }

  removeFromContext : Context → Context
  removeFromContext = proj₂ ∘ popFromCtxt

  popType : Context → Type₀ 
  popType Γ = recMaybe Unit AType (proj₁ (popFromCtxt Γ))

  AllowedScopeNarrowingTest : Scope → Scope → 𝟚
  AllowedScopeNarrowingTest s nothing = true
  AllowedScopeNarrowingTest s (just x) = caseMaybe true false s

  AllowedScopeNarrowing : (Γ : Context) → Scope → hProp ℓ-zero
  AllowedScopeNarrowing Γ =  Bool→Type' ∘ AllowedScopeNarrowingTest (scope' Γ) 


  narrowScope : (Γ : Context) → (s : Scope)  → (⟨ AllowedScopeNarrowing Γ s ⟩) → Scope
  narrowScope Γ s _ = caseMaybe s (scope' Γ) (Γ .scope') 
  
  narrow : (Γ : Context) → (s : Scope)  → (⟨ AllowedScopeNarrowing Γ s ⟩) → Context
  narrow Γ a x = record Γ { scope' = narrowScope Γ a x }



  data Stmnt (Γ : Context) : Type₀

  data BStmnt (Γ : Context) : Type₀


  data NBStmnt (Γ : Context) : Type₀

  data NBStmnt+Expr (Γ : Context) : Type₀

  data Expr (Γ : Context) (Τ : GType): Type₀

  bindingMechanics : {Γ : Context} → BStmnt Γ → List ContextEntry 

  bindingMechanics' : (Γ : Context) → Stmnt Γ → Context 


  record Body (Γ : _) (Τ : _ ) : Type₀ where
    pattern
    inductive
    constructor bodyR
    field
      stmnts : Linked' bindingMechanics' Γ
      expr : Expr (foldLinked' stmnts) Τ

  open Body public




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
    BS-publish! : (p : ParticipantId) → (PrivateSymbolOf Γ p)
                           → {_ : ⟨ IsConsensus Γ ⟩ }→  BStmnt Γ
    -- verify! ‹ids›

  data NBStmnt Γ where
    NBS-require! : Expr Γ Bool → NBStmnt Γ
    NBS-deposit! : ParticipantId → {_ : ⟨ IsConsensus Γ ⟩ } → Expr Γ Nat → NBStmnt Γ
    NBS-withdraw! : ParticipantId → {_ : ⟨ IsConsensus Γ ⟩ } → Expr Γ Nat → NBStmnt Γ


  data NBStmnt+Expr Γ where
    stmntNBS : NBStmnt Γ → NBStmnt+Expr Γ
    exprNBS : ∀ {Τ} → Expr Γ Τ → NBStmnt+Expr Γ

  bindingMechanics {Γ} (BS-let ce _) = ce ∷ Γ .entries
  bindingMechanics {Γ} (BS-publish! p x) =
    let makePublic  e = if (recMaybe false (λ p' → IdentifierTyTest (name p) (name p'))
                                     (scope e) and IdentifierTyTest (name e) (name x))
                            then (record e { scope = nothing })
                            else e

    in Cubical.Data.List.map makePublic (Γ .entries)


  bindingMechanics' Γ (bindingS x) = record Γ { entries =  bindingMechanics x } 
  bindingMechanics' Γ (nonBindingS x) = Γ

  Statements : Context → Type₀
  Statements Γ = Linked' bindingMechanics' Γ

ParticipantId' : InteractionHead → Type₀
ParticipantId' ie = Σ IdentifierTy λ n → ⟨ MemberBy IdentifierTyTest (ie .InteractionHead.participants) n ⟩

ParticipantId'-Iso : ∀ ih → Iso (ParticipantId' ih) (InteractionHead.ParticipantId ih)
Iso.fun (ParticipantId'-Iso ih) x = InteractionHead.pId (fst x) {snd x}
Iso.inv (ParticipantId'-Iso ih) x = InteractionHead.name x , InteractionHead.isIn x
Iso.rightInv (ParticipantId'-Iso ih) b = refl
Iso.leftInv (ParticipantId'-Iso ih) a = refl

ContextEntry' : InteractionHead → Type₀
ContextEntry' ie = (Maybe (ParticipantId' ie) )
                   × IdentifierTy × GType


-- ContextEntry'-Iso : ∀ ih → Iso (ContextEntry' ih) (InteractionHead.ContextEntry ih)
-- Iso.fun (ContextEntry'-Iso ih) x = InteractionHead.ice {!cong-Maybe ? proj₁ x!} {!!} {!!}
-- Iso.inv (ContextEntry'-Iso ih) = {!!}
-- Iso.rightInv (ContextEntry'-Iso ih) = {!!}
-- Iso.leftInv (ContextEntry'-Iso ih) = {!!}



-- PrivateSymbolOf' : ∀ {ih} → (defs : List (ContextEntry' ih))
--                      (p : ParticipantId' ih) →
--                      Type
-- PrivateSymbolOf' {ih} defs p =
--   Σ IdentifierTy λ x → fst (Bool→Type' (
--       recMaybe
--          false
--          (λ y → recMaybe false (λ p' → IdentifierTyTest (fst p) (fst p')) (proj₁ y))
--          (findBy (IdentifierTyTest x ∘ proj₁ ∘ proj₂ ) defs)))  

-- PrivateSymbolOf'= : ∀ {ih} → InteractionHead.PrivateSymbolOf {ih} ≡ {!PrivateSymbolOf' {ih}!}
-- PrivateSymbolOf'= = {!!}

lookup-findBy-lemma : ∀ {ℓ} {A : Type ℓ} → (l : List A) → (x : A) → (t₁ t₂ : A → A → 𝟚) → (t₁-comm : ∀ {x y} → t₁ x y ≡ t₁ y x ) → 
                         fst (Bool→Type' (recMaybe false (λ y → t₂ y x)
                              (findBy (λ x₂ → t₁ x x₂)
                               l)))
                          → fst (Bool→Type' (MemberByTest
                               (λ x y →
                                  t₁ x y and t₂ x y) l x))
lookup-findBy-lemma (x₂ ∷ l) x t₁ t₂ t₁-comm with dichotomyBool (t₁ x x₂)  
... | _⊎_.inr x₁ = λ e → let z = (subst (λ q → fst (Bool→Type' ( (recMaybe false (λ y → t₂ y x)
                                  (if q then just x₂ else findBy (t₁ x) l))))) x₁ e)
                             prev = lookup-findBy-lemma l x t₁ t₂ t₁-comm z

                             prevB = MemberByTest (λ x₃ y → t₁ x₃ y and t₂ x₃ y) l x
                          in 𝟚-byEq
                               {prevB}
                               { (t₁ x₂ x and t₂ x₂ x) or prevB}
                               ( sym (or-identityˡ prevB ) ∙ cong (_or prevB) (sym (F-and (t₂ x₂ x)) ∙ cong (_and _) (sym x₁ ∙ t₁-comm {x} {x₂})))
                              prev
... | _⊎_.inl p with dichotomyBool (t₂ x₂ x)
... | _⊎_.inl x₁ = λ _ → transport⁻ (Bool→Type'-≡ {(t₁ x₂ x and t₂ x₂ x or MemberByTest (λ x₃ y → t₁ x₃ y and t₂ x₃ y) l x)})
                       ((cong (_or (MemberByTest (λ x₃ y → t₁ x₃ y and t₂ x₃ y) l x)) (cong₂ _and_ (t₁-comm ∙ p) x₁) )
                         ∙ zeroˡ (MemberByTest (λ x₃ y → t₁ x₃ y and t₂ x₃ y) l x)) 
... | _⊎_.inr x₁ =
          
       λ yy →
          let kk = subst (λ b → fst (Bool→Type' (recMaybe false (λ y → t₂ y x) (if b then just x₂ else findBy (t₁ x) l)))) p yy
          in empty-rec ((subst (fst ∘ Bool→Type') x₁ kk))

toParamValue : ∀ {ie} → ParametersValue (ie .InteractionHead.parameters) → {Γ : InteractionHead.Context ie} → ∀ Τ s → 
        ⟨ Bool→Type' (caseMaybe true false (InteractionHead.entryLookup Γ s)  and (InteractionHead.IsMbShadowedParamOfTyTest Γ Τ s) ) ⟩
        → GTypeAgdaRep Τ
toParamValue {ie} x {Γ} Τ s x₁ =
  let z = proj₂ (and-True' {(caseMaybe true false (InteractionHead.entryLookup Γ s))} x₁)
  in lookup-ParametersValue (ie .InteractionHead.parameters) x (iwt s Τ)
     (lookup-findBy-lemma (ie .InteractionHead.parameters) ( iwt s Τ )
       (λ x₂ x₃ → IdentifierTyTest (name x₂) (name x₃))
       (λ x₂ x₃ → GTy== (type x₂) (type x₃))
       (λ {x} {y} → primStringEquality-comm {name x} {name y})
       z)
       

record Interaction : Type₀ where
  pattern
  constructor interaction
  field
    head : InteractionHead
    
  open InteractionHead head public

  field
    code : Linked' bindingMechanics' emptyContext 

-- open Interaction

infixl 6 interaction⟨_,_⟩_
infixr 50 _∶_ 

infixr 10 _;b_
infixr 15 _;_
infix 17 _;₁
infixr 15 _;'_

infix 30 set_∶_≔_
infix 30 at_set_∶_≔_


infix 60 <_>

pattern interaction⟨_,_⟩_ prts prms stmnts = interaction (interactionHead prts prms ) stmnts

pattern _∶_ x y = iwt x y 

pattern _;_ x y = _∷L_ x y


pattern _;₁ x = x ; []L  

pattern _;'_ x y = x ; y ;₁  

pattern set_∶_≔_ x y z =
  InteractionHead.bindingS (InteractionHead.BS-let (InteractionHead.ice nothing x y) z)

pattern at_set_∶_≔_ p x y z =
  InteractionHead.bindingS
     (InteractionHead.BS-let (InteractionHead.ice (just (InteractionHead.pId p)) x y) z)

pattern publish!_⟶_ x y = InteractionHead.bindingS (InteractionHead.BS-publish! (InteractionHead.pId x) (InteractionHead.psof y))

pattern deposit!_⟶_ x y = InteractionHead.nonBindingS (InteractionHead.stmntNBS (InteractionHead.NBS-deposit! (InteractionHead.pId x) y))

pattern withdraw!_⟵_ x y = InteractionHead.nonBindingS (InteractionHead.stmntNBS (InteractionHead.NBS-withdraw! (InteractionHead.pId x) y))

pattern require!_ x = InteractionHead.nonBindingS (InteractionHead.stmntNBS (InteractionHead.NBS-require! x))


<_> : ∀ {IH Γ} → {A : Type₀} → ⦃ isGlowTy : IsGlowTy A ⦄ →
         A →  InteractionHead.Expr IH Γ (IsGlowTy.glowRep isGlowTy)
<_> {IH} {Γ} {A} ⦃ isGlowTy ⦄ x = InteractionHead.lit (IsGlowTy.cast isGlowTy x)

pattern _;b_ x y = InteractionHead.body (InteractionHead.bodyR x y)  

infixr 60 v_

pattern v_ x = InteractionHead.var (InteractionHead.dsot x)



someInteraction : Interaction
someInteraction =
   interaction⟨   "A" ∷ "B" ∷ [] ,  "pI1" ∶ Nat ∷ "b2" ∶ Bool ∷ "b1" ∶ Bool ∷ [] ⟩ (
        set "x" ∶ Bool ≔ < true > ;
        at "B" set "y" ∶ Bool ≔ v "b1" ;
        at "A" set "xx" ∶ Bool ≔ (
            require! v "b2" ;'
            -- publish! "B" ⟶ "y" ;
            -- withdraw! "B" ⟵ < 3 > ;
            -- deposit! "B" ⟶ < 2 > ;
            set "z" ∶ Bool ≔ < false > ;b
            < true >
            );
        deposit! "B" ⟶ < 2 > ;
        withdraw! "B" ⟵ < 3 > ;
        publish! "B" ⟶ "y" ;'        
        set "yy" ∶ Bool ≔ v "y" )

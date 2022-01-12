
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
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool renaming (Bool to 𝟚)

open import Cubical.Data.Nat.Order.Recursive
open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

-- I am intentionaly avoiding "statement" and "expression" nomenclature to avodi confusion with previous layers
-- In the future we can intorduce it, or maybe on next level



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



IdentifierTy = String

IdentifierTyTest : IdentifierTy → IdentifierTy → 𝟚 
IdentifierTyTest = primStringEquality

Bool→Type' : 𝟚 → hProp ℓ-zero
Bool→Type' true = ⊤
Bool→Type' false = ⊥

Dec-Bool→Type' : ∀ {b} → ⟨ Decₚ (Bool→Type' b) ⟩
Dec-Bool→Type' {false} = no (idfun _)
Dec-Bool→Type' {true} = yes _


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
  if f x x'
   then  true
   else MemberByTest f xs x

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



    IsDefinedSymbolOfTyTest : GType → IdentifierTy → 𝟚
    IsDefinedSymbolOfTyTest ty x =
      let inParams =
            recMaybe false 
            (λ y → GTy== (type y) ty)
              (paramLookup x)

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

    AllowedScopeNarrowingTest : Scope → 𝟚
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
        at "B" set "y" ∶ Bool ≔ < true > ;
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



module paramsSubst where

  open InteractionHead

  -- paramsSubstH : ∀ {ih : _}  → ParametersValue (parameters ih) → Linked (bindingMechanics' ih) (emptyContext ih)
  --                 → let ih' = interactionHead (participants ih) []
  --                   in Linked (bindingMechanics' ih') (emptyContext ih') 
  -- paramsSubst {interactionHead ptcpnts []} pv x = x
  -- paramsSubst {interactionHead ptcpnts (x₁ ∷ prms)} pv x = {!!}

  stripParamsHead : InteractionHead → InteractionHead 
  stripParamsHead ih = interactionHead (participants ih) []

  stripCtxEntry : ∀ {ih : _} → ContextEntry ih → ContextEntry (stripParamsHead ih)
  stripCtxEntry x = ice (map-Maybe (λ x → pId (x .name) {x .isIn}) (x .scope)) (x .name) (x .type)


  partIdSubst : ∀ {ih : _} →  ParticipantId ih → ParticipantId (stripParamsHead ih) 
  partIdSubst x = pId (x .name) {x .isIn}


  stripParamsCtx : ∀ {ih : _} → Context ih → Context (stripParamsHead ih)
  stripParamsCtx Γ = 
    con (map-List stripCtxEntry (Γ .entries))
        (map-Maybe partIdSubst (Γ .scope'))

  stripParamsCtxConsensus : ∀ {ih : _} → {Γ : Context ih} → ⟨ IsConsensus Γ ⟩ → ⟨ IsConsensus (stripParamsCtx Γ) ⟩ 
  stripParamsCtxConsensus {Γ = con entries₁ nothing} x = tt*




  w22 : ∀ {ih} → (ents : List (ContextEntry ih)) →  {p : ParticipantId ih}
          → (ss : IdentifierTy) →
        fst
        (Bool→Type'
         (recMaybe false
          (λ y →
             recMaybe false (λ p' → primStringEquality (name p) (name p'))
             (scope { ih} y))
          (findBy (λ x₁ → primStringEquality ss (name x₁))
           ents))) →
        fst
        (Bool→Type'
         (recMaybe false
          (λ y →
             recMaybe false (λ p' → primStringEquality (name (partIdSubst p)) (name p'))
             (scope {stripParamsHead ih} y))
          (findBy (λ (x₁ : ContextEntry (stripParamsHead ih)) → primStringEquality ss (name x₁))
           (map-List stripCtxEntry 
            ents))))
  w22 (ice po name₁ type₁ ∷ ents) {p} ss =  
    let ff = w22 ents {p} ss 
        tt = 
             maybe-elim {B = λ po → fst
                     (Bool→Type'
                      (recMaybe false (λ p' → primStringEquality (name p) (name p'))
                       po)) →
                     fst
                     (Bool→Type'
                      (recMaybe false
                       (λ p' → primStringEquality (name (partIdSubst p)) (name p'))
                       (scope (stripCtxEntry (ice po name₁ type₁)))))} (idfun _) (λ a → idfun _) po
    in 𝟚-elim
        {A = λ b →
        (fst
      (Bool→Type'
       (recMaybe false
        (λ y →
           recMaybe false (λ p' → primStringEquality (name p) (name p'))
           (scope y))
        (if b then
         just (ice po name₁ type₁) else
         findBy (λ x₁ → primStringEquality ss (name x₁)) ents))))
        → (fst
      (Bool→Type'
       (recMaybe false
        (λ y →
           recMaybe false
           (λ p' → primStringEquality (name (partIdSubst p)) (name p'))
           (scope y))
        (if b
         then just (stripCtxEntry (ice po name₁ type₁)) else
         findBy (λ x₁ → primStringEquality ss (name x₁))
         (map-List stripCtxEntry ents)))))}
       ff tt (primStringEquality ss
         (name (stripCtxEntry (ice po name₁ type₁))))

  stripParamsPrivateSymbol : ∀ {ih : _} → {Γ : Context ih} → ∀ {p} → PrivateSymbolOf Γ p → PrivateSymbolOf (stripParamsCtx Γ) (partIdSubst p) 
  stripParamsPrivateSymbol {Γ = Γ} {p} x = psof (x .name) {w22 (Γ .entries) {p} (x .name) (x .isDefinedSymbolOf)}



  -- paramsSubst : ∀ {ih : _} → ∀ {Γ : _} → ParametersValue (parameters ih) → Linked' (bindingMechanics' ih) Γ
  --                 → let ih' = interactionHead (participants ih) []
  --                       Γ' = stripParamsCtx Γ
  --                   in Linked' (bindingMechanics' ih') Γ' 
  -- paramsSubst {interactionHead ptcpnts []} pv x = {!x!}
  -- paramsSubst {interactionHead ptcpnts (x₁ ∷ prms)} pv []L = []L
  -- paramsSubst {interactionHead ptcpnts (x₁ ∷ prms)} pv (h ; x) =
  --       let z = {!!} -- paramsSubst {interactionHead ptcpnts (x₁ ∷ prms)} pv {!x!}
  --       in {!!}


  -- parramsIrrForCtxFun : ∀ {ih : _} → (Context ih) → (Context (stripParamsHead ih))
  -- parramsIrrForCtxFun (con entries₁ scope'') =
  --      con (map-List (λ (ice x y z) → ice (map-Maybe (λ (pId x {y}) → pId x {y}) x) y z) entries₁)
  --                                             (map-Maybe (λ (pId x {y}) → pId x {y}) scope'')

  -- TODO : (easy), fix signature, assume same participants
  -- parramsIrrForCtx : ∀ {ih : _} → {ps' : _} → {uPams : _} → let ih' = interactionHead (participants ih) (ps') {uPams} in Iso (Context ih) (Context ih')
  -- Iso.fun parramsIrrForCtx (con entries₁ scope'') =
  --      con (map-List (λ (ice x y z) → ice (map-Maybe (λ (pId x {y}) → pId x {y}) x) y z) entries₁)
  --                                             (map-Maybe (λ (pId x {y}) → pId x {y}) scope'')
  -- Iso.inv parramsIrrForCtx(con entries₁ scope'')  =
  --      con (map-List (λ (ice x y z) → ice (map-Maybe (λ (pId x {y}) → pId x {y}) x) y z) entries₁)
  --                                             (map-Maybe (λ (pId x {y}) → pId x {y}) scope'')
  -- Iso.rightInv parramsIrrForCtx (con entries₁ scope'') i = {!!}
  -- Iso.leftInv parramsIrrForCtx = {!!}

  -- -- TODO : remove unsafe pragma

  {-# TERMINATING #-}
  paramSubst : ∀ {ih : _} → ParametersValue (parameters ih) → 
                   ∀ {Γ : _} →  Linked' (bindingMechanics' ih) Γ → Linked' (bindingMechanics' (stripParamsHead ih)) (stripParamsCtx Γ) 

  wwww : ∀ {ih} {vv : ParametersValue (parameters ih)}
           {Γ : Context ih} 
           (stmnts₁  : Linked' (bindingMechanics' ih) Γ)
            →
         stripParamsCtx (foldLinked' stmnts₁) ≡
         foldLinked' (paramSubst vv stmnts₁)



  paramSubst {ih}  vv = map-Linked'-map _ h hh
    where



      h : {Γ : Context ih}
             → (b : Stmnt ih Γ) → Stmnt _ (stripParamsCtx Γ)

      h' : {Γ : Context ih} → ∀ {Τ}
             → (b : Expr ih Γ Τ) → Expr _ (stripParamsCtx Γ) Τ


      h'' : {Γ : Context ih} →  BStmnt ih Γ -> BStmnt _ _


      h'' {Γ} (BS-let x {asn} y) = BS-let (ice (map-Maybe (λ x → pId (x .name) {x .isIn}) (x .scope)) (x .name) (x .type))
                                  {{!h-narrowing Γ x {asn}!}} (subst (λ x₁ → Expr (stripParamsHead ih) x₁ (type x)) {!!} (h' y))


      h'' {Γ} (BS-publish! p x {y}) = BS-publish! (partIdSubst p) (stripParamsPrivateSymbol x) {stripParamsCtxConsensus {ih} {Γ} y}


      h  (bindingS x) = bindingS (h'' x)

      h {Γ} (nonBindingS x) = nonBindingS (z x)
         where

           zz : NBStmnt _ _ → NBStmnt _ _ 
           zz (NBS-require! x) = NBS-require! (h' x)
           zz (NBS-deposit! p {y} x) = NBS-deposit! (partIdSubst p) {stripParamsCtxConsensus {ih} {Γ}  y} (h' x)
           zz (NBS-withdraw! p {y} x) = NBS-withdraw! (partIdSubst p) {stripParamsCtxConsensus {ih} {Γ} y} (h' x)

           z : NBStmnt+Expr ih _ → NBStmnt+Expr (stripParamsHead ih) _
           z (stmntNBS x) =  stmntNBS (zz x)
           z (exprNBS x) = exprNBS (h' x)




      h' (var x) = {!!}
      h' (stmnts₁ ;b x) =  paramSubst vv stmnts₁ ;b subst (λ x₁ → Expr _ x₁ _) (wwww stmnts₁ ) (h' x)
      h' (lit x) = lit x

      hh : {Γ : Context ih} {x : Stmnt ih Γ} →
         stripParamsCtx (bindingMechanics' ih Γ x) ≡
         bindingMechanics' (interactionHead (participants ih) [])
         (stripParamsCtx Γ) (h x)
      hh {Γ} {x = bindingS (BS-let ce x)} = 
        cong (λ q → con q (map-Maybe (λ x → pId (x .name) {x .isIn}) (Γ .scope'))) refl
      hh {Γ} {x = bindingS (BS-publish! p x)} = 
        cong (λ q → con q _) (map-List-∘ _ _ _ ∙ cong (λ a → map-List a (Γ .entries)) (funExt qqq) ∙ (sym (map-List-∘ _ _ _))   )
        where
          qqq : _
          qqq = {!!}
          -- qqq (ice (just x) name₁ type₁) = {!!}
 
        -- where
        --   hh' : (x : BStmnt ih Γ) →
        --        map-List
        --        (λ x₁ →
        --           Interaction.ice (map-Maybe (λ x₂ → x₂) (x₁ .scope)) (x₁ .name)
        --           (x₁ .type))
        --        (bindingMechanics' ih Γ (Interaction.bindingS x) .entries)
        --        ≡ {!!}

        --   hh' (BS-let ce x) = {!!}
        --   hh' (BS-publish! p x) = {!!}
      hh {x = nonBindingS x} = refl

  wwww {ih} = map-Linked'-map-fold (stripParamsCtx {ih}) _ _ 

  -- paramSubst {interactionHead participants₁ parameters₁} {p = name₁ ∶ type₁} vv (bindingS (BS-let (ice scope name₂ type₂) x₁) ; x) = 
  --    (bindingS (BS-let ((ice {!!} name₂ type₂)) {!!}) ; paramSubst vv {!x!})
  -- paramSubst vv (bindingS (BS-publish! p x₁) ; x) = {!!}
  -- paramSubst vv (nonBindingS x₁ ; x) = nonBindingS {!!} ; paramSubst vv x


  --  L[] = L[]
  -- paramsSubst {interactionHead ptcpnts (x₁ ∷ prms)} pv (x ; x₂) =
  --   let z = paramsSubst {interactionHead ptcpnts (x₁ ∷ prms)} pv x
  --   in z ; {!x₂!}


{-# OPTIONS --cubical  #-}
module Glow.Simple.AST where

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

open import Glow.Simple.Postulates

open import Glow.ListDecProps

open import Cubical.Categories.Category

data GType : Type₀ where
  Bool : GType 
  Int : GType
  Nat : GType
  Unitᵍ : GType
  Digest : GType
  Signature : GType






-- GTy≟ : Discrete GType
-- GTy≟ x y = {!x y!}

GTy== : GType → GType → 𝟚
GTy== Bool Bool = true
GTy== Int Int = true
GTy== Nat Nat = true
GTy== Unitᵍ Unitᵍ = true
GTy== Digest Digest = true
GTy== Signature Signature = true
GTy== _ _ = false



instance
  IsDiscrete-GType : IsDiscrete GType
  eqTest IsDiscrete-GType Bool Bool = yes refl
  eqTest IsDiscrete-GType Bool Int = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Bool Nat = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Bool Unitᵍ = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Int Bool = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Int Int = yes refl
  eqTest IsDiscrete-GType Int Nat = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Int Unitᵍ = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Nat Bool = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Nat Int = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Nat Nat = yes refl
  eqTest IsDiscrete-GType Nat Unitᵍ = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Unitᵍ Bool = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Unitᵍ Int = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Unitᵍ Nat = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Unitᵍ Unitᵍ = yes refl
  eqTest IsDiscrete-GType Digest Digest = yes refl
  eqTest IsDiscrete-GType Signature Signature = yes refl
  eqTest IsDiscrete-GType Bool Digest = no-dec-eq-help' GTy== _ 
  eqTest IsDiscrete-GType Bool Signature = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Int Digest = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Int Signature = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Nat Digest = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Nat Signature = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Unitᵍ Digest = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Unitᵍ Signature = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Digest Bool = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Digest Int = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Digest Nat = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Digest Unitᵍ = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Digest Signature = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Signature Bool = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Signature Int = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Signature Nat = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Signature Unitᵍ = no-dec-eq-help' GTy== _
  eqTest IsDiscrete-GType Signature Digest = no-dec-eq-help' GTy== _

GTypeAgdaRep : GType → Type₀
GTypeAgdaRep Bool = 𝟚
GTypeAgdaRep Int = ℤ
GTypeAgdaRep Nat = ℕ
GTypeAgdaRep Unitᵍ = Unit
GTypeAgdaRep Digest = Dig
GTypeAgdaRep Signature = Sig


isSet-GType : isSet GType
isSet-GType = Discrete→isSet (IsDiscrete.eqTest IsDiscrete-GType)

record IsGlowTy (A : Type₀) : Type₀ where
  field
    glowRep : GType
    -- glowRep-coh : A ≡ GTypeAgdaRep glowRep
    cast : A → GTypeAgdaRep glowRep
    isSet-A : isSet A
    
instance
  Bool-IsGlowTy : IsGlowTy 𝟚
  Bool-IsGlowTy = record { glowRep = Bool ; cast = idfun _ ; isSet-A = isSetBool}

instance
  ℤ-IsGlowTy : IsGlowTy ℤ
  ℤ-IsGlowTy = record { glowRep = Int  ; cast = idfun _  ; isSet-A = isSetℤ}

instance
  ℕ-IsGlowTy : IsGlowTy ℕ
  ℕ-IsGlowTy = record { glowRep = Nat  ; cast = idfun _  ; isSet-A = isSetℕ}

instance
  Unit-IsGlowTy : IsGlowTy Unit
  Unit-IsGlowTy = record { glowRep = Unitᵍ  ; cast = idfun _  ; isSet-A = λ x y x₁ y₁ i i₁ → _}

instance
  Dig-IsGlowTy : IsGlowTy Dig
  Dig-IsGlowTy = record { glowRep = Digest  ; cast = idfun _  ; isSet-A = isSet-Dig}

instance
  Sig-IsGlowTy : IsGlowTy Sig
  Sig-IsGlowTy = record { glowRep = Signature  ; cast = idfun _  ; isSet-A = isSet-Sig}


GTypeAgdaRep' : (Τ : GType) → IsGlowTy (GTypeAgdaRep Τ) 
GTypeAgdaRep' Bool = Bool-IsGlowTy
GTypeAgdaRep' Int = ℤ-IsGlowTy
GTypeAgdaRep' Nat = ℕ-IsGlowTy
GTypeAgdaRep' Unitᵍ = Unit-IsGlowTy
GTypeAgdaRep' Digest = Dig-IsGlowTy
GTypeAgdaRep' Signature = Sig-IsGlowTy

record GlowValue : Type₀ where
  field
    gType : GType
    gValue : GTypeAgdaRep gType
    


subst-GTypeAgdaRep : ∀ {Τ₀ Τ₁} → Τ₀ ≡ Τ₁ → GTypeAgdaRep Τ₀ → GTypeAgdaRep Τ₁
subst-GTypeAgdaRep {Τ₀} {Τ₁} = h _ _ ∘ fromWitness
  where
    h : ∀ Τ₀ Τ₁ → True (Τ₀ ≟ Τ₁) → GTypeAgdaRep Τ₀ → GTypeAgdaRep Τ₁
    h Bool Bool tt = idfun _
    h Int Int tt = idfun _
    h Nat Nat tt = idfun _
    h Unitᵍ Unitᵍ tt = idfun _
    h Digest Digest tt = idfun _
    h Signature Signature tt = idfun _


-- data GFunType : Type₀ where
--   _G→_ : List GType → GType → GFunType

argsV : List GType → Type₀
argsV [] = Unit
argsV (x ∷ x₁) = GTypeAgdaRep x × argsV x₁ 


funV : List GType → GType → Type₀
funV [] x₁ = GTypeAgdaRep x₁
funV (x ∷ x₂) x₁ = GTypeAgdaRep x → funV x₂ x₁

appV : ∀ {dm cdm} → funV dm cdm → argsV dm → GTypeAgdaRep cdm
appV {[]} x x₁ = x
appV {x₂ ∷ dm} x (x₁ , x₃) = appV (x x₁) x₃


module _ (BuilitInsIndex : Type₀) {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}} where
  record BuiltIn' (dm : List GType) (cdm : GType)  : Type₀ where
    constructor builitIn
    field
      impl : funV dm cdm 

  record BuiltIns' : Type₀ where
    field
      getBi : BuilitInsIndex → Σ _ λ x → BuiltIn' (proj₁ x) (proj₂ x)



ParticipantModality : Type₀
ParticipantModality = 𝟚

honest dishonest : ParticipantModality 
honest = true
dishonest = false


module _ (Identifier : Type₀) {{IsDiscrete-Identifier : IsDiscrete Identifier}}
           {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              (builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}) where

  module AST (prop-mode : Interval) where 

    open PropMode prop-mode

    BuiltIn = BuiltIn' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}} 

    open BuiltIns' builtIns


    record BI₀ (dm : List GType) (cdm : GType ) : Type₀ where
      constructor bi'₀
      field
        bIndex : BuilitInsIndex
        {dm≡} : dm PM≡ proj₁ (fst (getBi bIndex))
        {cdm≡} : cdm PM≡ proj₂ (fst (getBi bIndex))


    record BI (cdm : GType ) : Type₀ where
      constructor bi'
      field
        bIndex : BuilitInsIndex
        -- {dm≡} : dm PM≡ proj₁ (fst (getBi bIndex))
        {cdm≡} : cdm PM≡ proj₂ (fst (getBi bIndex))


    getBI-Dm : ∀ {cdm} → BI cdm → List GType 
    getBI-Dm x = proj₁ (fst (getBi (BI.bIndex x )))

    bi : (x : BuilitInsIndex) → BI ((proj₂ (fst (getBi x))))
    bi x = bi' x {toWitness'bck refl}

    isSetIdentifier = Discrete→isSet (IsDiscrete.eqTest IsDiscrete-Identifier)


    record IdentifierWithType : Type₀ where
      pattern
      constructor iwt
      field
        name : Identifier
        type : GType

    open IdentifierWithType

    instance
      IdentifierWithType-Discrete : IsDiscrete IdentifierWithType
      eqTest IdentifierWithType-Discrete x y =
        dec-rec ((x .name ≡ y .name) × (x .type ≡ y .type))

          (λ x₁ → yes λ i → iwt (proj₁ x₁ i) (proj₂ x₁ i)) λ x₁ → no λ x₂ → x₁ ((λ i → name (x₂ i)) , (λ i → (type (x₂ i))))

    isSet-IdentifierWithType : isSet IdentifierWithType
    isSet-IdentifierWithType = Discrete→isSet (IsDiscrete.eqTest IdentifierWithType-Discrete)

    ParametersValue : List IdentifierWithType →  Type₀
    ParametersValue [] = Unit
    ParametersValue (x ∷ xs) = GTypeAgdaRep (type x) × ParametersValue xs

    lookup-ParametersValue : (l : List IdentifierWithType) → ParametersValue l
                               → (x : IdentifierWithType)
                               → IsMemberOf x l 
                               → GTypeAgdaRep (type x)
    lookup-ParametersValue (x₃ ∷ l) (x₁ , x₂) x ex =
       dec-rec (x ≡ x₃)
          (λ p → subst-GTypeAgdaRep (cong type (sym p)) x₁)
          (λ ¬p → lookup-ParametersValue l x₂ x (ExistMemberAs-¬head→tail ex ¬p)) -- ?

    IsParticipantId : {participants : List Identifier} → Identifier → DecPropΣ 
    IsParticipantId {participants} name =
        ExistMemberAs (name ≡_) participants
          , ?? _ , Is-Prop-ExistMemberAs _ _ (isSetIdentifier _)

    IsHonestParticipantId : {participants : List (Identifier × ParticipantModality)} → Identifier → DecPropΣ 
    IsHonestParticipantId {participants} name =
        let q : (Identifier × ParticipantModality) → Σ Type (λ x → Dec x × isProp x)
            q = (λ x → (×-dp (name DP≡ (proj₁ x)) (honest DP≡ (proj₂ x)))  )
        in ExistMemberAs (λ x → fst (q x)  ) participants
              , Dec-ExistMemberAs {{dec-pred (λ x → proj₁ (snd (q x)))}}
                , Is-Prop-ExistMemberAs _ _ (λ x → proj₂ (snd (q x)))


    IsDishonestParticipantId : {participants : List (Identifier × ParticipantModality)} → Identifier → DecPropΣ 
    IsDishonestParticipantId {participants} name =
        let q : (Identifier × ParticipantModality) → Σ Type (λ x → Dec x × isProp x)
            q = (λ x → (×-dp (name DP≡ (proj₁ x)) (dishonest DP≡ (proj₂ x)))  )
        in ExistMemberAs (λ x → fst (q x)  ) participants
              , Dec-ExistMemberAs {{dec-pred (λ x → proj₁ (snd (q x)))}}
                , Is-Prop-ExistMemberAs _ _ (λ x → proj₂ (snd (q x)))



    -- IsHonest : {participantsWH : List (Identifier × ParticipantModality)}
    --               → (x : Identifier) → PM ( IsParticipantId {map-List proj₁ participantsWH} x )
    --               → DecPropΣ 
    -- IsHonest x x₁ = {!
    -- !}

    data ParticipantId' {participants : List Identifier} : Type₀ where
      pId : (name : Identifier) → {isIn :  PM ( IsParticipantId {participants} name ) } → ParticipantId'

    data HonestParticipantId' {participants : List (Identifier × ParticipantModality)} : Type₀ where
      pId : (name : Identifier) → {isIn :  PM ( IsHonestParticipantId {participants} name ) } → HonestParticipantId'

    data DishonestParticipantId' {participants : List (Identifier × ParticipantModality)} : Type₀ where
      pId : (name : Identifier) → {isIn :  PM ( IsDishonestParticipantId {participants} name ) } → DishonestParticipantId'


    pId-name : ∀ {ptps} → ParticipantId' {ptps} → Identifier
    pId-name (pId name₁) = name₁

    pId-nameHon : ∀ {ptps} → HonestParticipantId' {ptps} → Identifier
    pId-nameHon (pId name₁) = name₁

    pId-isInHon : ∀ {ptps} → (hp : HonestParticipantId' {ptps}) → PM ( IsHonestParticipantId {ptps} (pId-nameHon hp))
    pId-isInHon (pId _ {y}) = y
    
    pId-nameDishon : ∀ {ptps} → DishonestParticipantId' {ptps} → Identifier
    pId-nameDishon (pId name₁) = name₁

    pId-isInDishon : ∀ {ptps} → (hp : DishonestParticipantId' {ptps}) → PM ( IsDishonestParticipantId {ptps} (pId-nameDishon hp))
    pId-isInDishon (pId _ {y}) = y


    -- record ParticipantId' {participants : List Identifier} : Type₀ where
    --   constructor pId

    --   field
    --     name : Identifier
    --     {isIn} : True (snd (IsParticipantId {participants} name))

    open ParticipantId' public


    Scope' : {participants : List (Identifier × ParticipantModality)} → Type₀
    Scope' {participants} = Maybe (HonestParticipantId' {participants})

    _CanAccess_ : ∀ {ps} → Scope' {ps} → Scope' {ps} → DecPropΣ
    _ CanAccess nothing = Unit , ?? _ , λ x y i → tt
    just x CanAccess just x₁ = ((pId-nameHon x) DP≡ (pId-nameHon x₁))
    nothing CanAccess just x₁ = Empty , ?? _ , isProp⊥

    AllowedScopeNarrowing' : ∀ {ps} → Scope' {ps} → Scope' {ps} → DecPropΣ
    AllowedScopeNarrowing' s nothing = Unit , yes _ , λ x y i → tt
    AllowedScopeNarrowing' s (just x) =
       caseMaybe
          Unit-dp
          Empty-dp
           s



    record ContextEntry' {participantsWM : List (Identifier × ParticipantModality)} : Type₀ where
      constructor ice

      field
        scope : Scope' {participantsWM}
        name : Identifier
        type : GType




    open ContextEntry' public


    

    record InteractionHead : Type₀ where
      constructor interactionHead
      -- inductive
      pattern
      field
        participantsWM : List (Identifier × ParticipantModality)
        parameters : List IdentifierWithType
        {uniqueParams} : PM (_ , UniqueByDec≡ name parameters , isProp-UniqueBy _ _ )
        {uniquePtcpnts} : PM (_ , UniqueByDec≡ proj₁ participantsWM , isProp-UniqueBy _ _ )

      participants = map-List proj₁ participantsWM



      ParticipantId : Type₀
      ParticipantId = ParticipantId' {participants}

      HonestParticipantId : Type₀
      HonestParticipantId = HonestParticipantId' {participantsWM}

      DishonestParticipantId : Type₀
      DishonestParticipantId = DishonestParticipantId' {participantsWM}


      DishonestParticipantId→ℕ : DishonestParticipantId → ℕ
      DishonestParticipantId→ℕ (pId _ {x}) = where?-ExistMemberAs (toWitness' x)


      Scope : Type₀
      Scope = Maybe HonestParticipantId

      ContextEntry = ContextEntry' {participantsWM}

      AType : ContextEntry → Type₀
      AType ce = GTypeAgdaRep (ce .type)

      AhSet : ContextEntry → hSet ℓ-zero
      AhSet ce = GTypeAgdaRep (ce .type) , IsGlowTy.isSet-A (GTypeAgdaRep' (ce .type))

      ce-name : ContextEntry → Identifier
      ce-name = ContextEntry'.name

      ce-scope : ContextEntry → Scope 
      ce-scope = ContextEntry'.scope

      record Context : Type₀ where
        pattern
        constructor con


        field
          entries : List ContextEntry

        field
          scope' : Scope



        IsDefinedVariableOfTy : GType → Identifier → DecPropΣ
        IsDefinedVariableOfTy ty x =
          ExistFirstBy ((x ≡_) ∘ name) 
             WitchIsAlso (λ y → ⟨ scope' CanAccess (scope y) ⟩ × (ty ≡ type y) ) entries
           , Dec-ExistFirstBy_WitchIsAlso {{Dec-Pred-B' = dec-pred λ y → ×-Dec {{proj₁ (snd (scope' CanAccess (scope y)))}}}}
              , ExistFirstBy-WitchIsAlso-isProp _ (λ x₁ → isSetIdentifier _ _)
                   λ y _ _ → ×≡ (proj₂ (snd (scope' CanAccess (scope y))) _ _) (isSet-GType _ _ _ _)  

        IsNotShadowedParamOfTy : GType → Identifier → Type ℓ-zero
        IsNotShadowedParamOfTy ty x =
           IsEmpty (ExistMemberAs ((x ≡_) ∘ name) entries)
              × IsMemberOf (iwt x ty) parameters      


        IsDefinedSymbolOfTy : GType → Identifier → DecPropΣ
        IsDefinedSymbolOfTy ty x = 
          ⟨ IsDefinedVariableOfTy ty x ⟩ ⊎ IsNotShadowedParamOfTy ty x ,
            ⊎-Dec {{proj₁  (snd ((IsDefinedVariableOfTy ty x))) }} ,
              ⊎-isProp (proj₂  (snd ((IsDefinedVariableOfTy ty x)))) 
                       (λ x₁ y → ×≡ (isProp¬ _ _ _) (Is-Prop-ExistMemberAs _ _ (λ x₂ x₃ y₁ → isSet-IdentifierWithType _ _ _ _) _ _))
                λ x₁ x₂ → proj₁ x₂ (ExistFirstByWitchIsAlso→ExistMemberAs _ x₁)


        data DefinedSymbolOfTy (Τ : GType) : Type ℓ-zero where
          dsot : (name : Identifier) → {isDefinedSymbolOfTy : PM ( IsDefinedSymbolOfTy Τ name ) } → DefinedSymbolOfTy Τ

        open DefinedSymbolOfTy public


        IsPrivateSymbolOf : HonestParticipantId → Identifier → DecPropΣ
        IsPrivateSymbolOf p x = 
           ExistFirstBy ((x ≡_) ∘ name)
              WitchIsAlso (λ y → recMaybe Empty (λ p' → (pId-nameHon p) ≡ (pId-nameHon p')) (scope y)) entries
             , Dec-ExistFirstBy_WitchIsAlso {{Dec-Pred-B' = Dec-Pred-Maybe {f = scope}}}
               , ExistFirstBy-WitchIsAlso-isProp _ (λ x₁ → isSetIdentifier _ _)
                  λ y → recMaybe-Empty-isProp ((λ x₁ → isSetIdentifier _ _)) (scope y)

        data PrivateSymbolOf (p : HonestParticipantId) : Type ℓ-zero where
          psof : (name : Identifier) → {isDefinedSymbolOf : PM ( IsPrivateSymbolOf p name ) } → PrivateSymbolOf p 

        psof-name : ∀ {p} → PrivateSymbolOf p → Identifier
        psof-name (psof x) = x 

        psof-proof : ∀ {p} → (pso : PrivateSymbolOf p) → PM ( IsPrivateSymbolOf p (psof-name pso) )
        psof-proof (psof x {y}) = y 



        open PrivateSymbolOf public



        IsConsensus : DecPropΣ
        IsConsensus = caseMaybe (Unit , yes _ , λ x y i → tt ) (Empty , no (idfun _) , isProp⊥) scope'

        IsNotConsensus : DecPropΣ
        IsNotConsensus = caseMaybe (Empty , no (idfun _) , isProp⊥ ) (Unit , yes _ , λ x y i → tt)  scope'




      open Context public

      postulate isSet-Context : isSet Context
      -- isSet-Context (con entries₁ scope'') (con entries₁* scope''*) p q i i₁ =
      --    con (isOfHLevelList 0 {!!} _ _ (cong entries p) (cong entries q) i i₁ )
      --        (isOfHLevelMaybe 0 {!!} _ _ (cong scope' p) (cong scope' q) i i₁)


      IsPrivateSymbolOf→GType : (Γ : Context) → ∀ hp → ∀ nm
                         → ⟨ IsPrivateSymbolOf Γ hp nm ⟩ → GType
      IsPrivateSymbolOf→GType (con (ice scope name type ∷ entries₁) scope'') hp nm (inl x) = type
      IsPrivateSymbolOf→GType (con (x₁ ∷ entries₁) scope'') hp nm (inr x) =
        IsPrivateSymbolOf→GType (con (entries₁) scope'') hp nm (proj₂ x)



      IsNotConsensus→Participant : ∀ {Γ} → PM (IsNotConsensus Γ) → HonestParticipantId
      IsNotConsensus→Participant {con entries₁ nothing} x = empty-elim (toWitness' x)
      IsNotConsensus→Participant {con entries₁ (just x₁)} x = x₁


      -- context-< : Context → ℕ → Type₀ 
      -- context-< x x₁ = {!!}

      emptyContext : Context
      emptyContext = con [] nothing

      prependContext : ContextEntry → Context →  Context
      prependContext x Γ = record Γ { entries =   Γ .entries ∷ʳ x } 

      addToContext : Context → ContextEntry → Context
      addToContext Γ x = record Γ { entries =  x ∷ Γ .entries } 


      removeFromContext' : ∀ (Γ : _) → ∀ s → ∀ Τ → ⟨ IsDefinedVariableOfTy Γ Τ s ⟩ → List ContextEntry
      removeFromContext' (con (x₁ ∷ entries₁) scope'') s Τ (inl x) = entries₁
      removeFromContext' (con (x₁ ∷ entries₁) scope'') s Τ (inr x) =  (x₁ ∷ removeFromContext' (con (entries₁) scope'') s Τ (proj₂ x) )

      removeFromContext : ∀ (Γ : _) → ∀ s → ∀ Τ → ⟨ IsDefinedVariableOfTy Γ Τ s ⟩ → Context
      removeFromContext Γ s Τ x = record Γ { entries =  removeFromContext' Γ s Τ x } 


      AllowedScopeNarrowing : (Γ : Context) → Scope → DecPropΣ
      AllowedScopeNarrowing Γ = AllowedScopeNarrowing' (scope' Γ) 


      narrowScope : (Γ : Context) → (s : Scope)  → PM (AllowedScopeNarrowing Γ s) → Scope
      narrowScope Γ s _ = caseMaybe s (scope' Γ) (Γ .scope') 

      narrow : (Γ : Context) → (s : Scope)  → (PM  (AllowedScopeNarrowing Γ s) ) → Context
      narrow Γ a x = record Γ { scope' = narrowScope Γ a x }

      -- TODO : put type annotations in special additional , parametrized field
      module Unsafe where


        data Stmnt : Type₀

        data BStmnt : Type₀


        data NBStmnt : Type₀

        data NBStmnt+Expr : Type₀

        data Expr : Type₀

        data Arg : Type₀

        Args = List Arg


        record Body : Type₀ where
          pattern
          inductive
          constructor bodyR
          field
            stmnts : List (Stmnt)
            expr : Expr

        open Body public

        data Arg where
          var-a : Identifier → Arg
          lit-a : GlowValue → Arg


        data Expr where
          var : GType → Identifier → Expr
          body : Body → Expr
          lit : GlowValue → Expr
          _$'_ : BuilitInsIndex → Args → Expr
          input : GType → String → Expr
          sign : Arg → Expr 


          -- -- this is temporary solution, this constructors cannot apear in code, and are introduced on some passes, this distinction must be typesafe in the future! 
          receivePublished : GType → DishonestParticipantId →  Expr

          if_then_else_ : Expr → Expr → Expr → Expr

        data Stmnt where
          -- -- not necessary binding, but rather context changing
          bindingS : BStmnt → Stmnt
          nonBindingS : NBStmnt+Expr → Stmnt

        data BStmnt where
          BS-let : Maybe HonestParticipantId → Identifier → GType → Expr → BStmnt    
          BS-publish! : HonestParticipantId → GType → Identifier →  BStmnt

        data NBStmnt where
          NBS-require! : Expr → NBStmnt
          NBS-deposit! : ParticipantId → Expr → NBStmnt
          NBS-withdraw! : ParticipantId → Expr → NBStmnt
          -- this is temporary solution, this constructors cannot apear in code, and are introduced on some passes, this distinction must be typesafe in the future!

          NBS-publishVal! : HonestParticipantId → Identifier → NBStmnt

        data NBStmnt+Expr where
          stmntNBS : NBStmnt → NBStmnt+Expr
          exprNBS : Expr → NBStmnt+Expr

        Τ? : Expr → GType
        Τ? (var x x₁) = x
        Τ? (body (bodyR _ e)) = Τ? e
        Τ? (lit record { gType = gType ; gValue = gValue }) = gType
        Τ? (x $' x₁) = proj₂ (fst (getBi x))
        Τ? (input x x₁) = x
        Τ? (sign x) = Signature
        Τ? (receivePublished x x₁) = x
        Τ? (if x then x₁ else x₂) = Τ? x₁

      data Stmnt (Γ : Context) : Type₀

      data BStmnt (Γ : Context) : Type₀


      data NBStmnt (Γ : Context) : Type₀

      data NBStmnt+Expr (Γ : Context) : Type₀

      data Expr (Γ : Context) (Τ : GType): Type₀

      data Arg (Γ : Context) (Τ : GType): Type₀

      Args : (Γ : Context) (Τs : List GType) → Type₀


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

      data Arg Γ Τ where
        var-a : DefinedSymbolOfTy Γ Τ → Arg Γ Τ
        lit-a : GTypeAgdaRep Τ → Arg Γ Τ


      data Expr Γ Τ where
        var : DefinedSymbolOfTy Γ Τ → Expr Γ Τ
        body : Body Γ Τ → Expr Γ Τ
        lit : GTypeAgdaRep Τ → Expr Γ Τ
        _$'_ : (x : BI Τ) → Args Γ (getBI-Dm x) → Expr Γ Τ
        input : String → {_ : PM (IsNotConsensus Γ) } → Expr Γ Τ
        sign : Arg Γ Digest → {_ : PM (IsNotConsensus Γ) } → {_ : Signature PM≡ Τ} → Expr Γ Τ
        

        -- this is temporary solution, this constructors cannot apear in code, and are introduced on some passes, this distinction must be typesafe in the future! 
        receivePublished : DishonestParticipantId → {_ : PM (IsConsensus Γ) } → Expr Γ Τ

        if_then_else_ : Expr Γ Bool → Expr Γ Τ → Expr Γ Τ → Expr Γ Τ

      data Stmnt Γ where
        -- not necessary binding, but rather context changing
        bindingS : BStmnt Γ → Stmnt Γ
        nonBindingS : NBStmnt+Expr Γ → Stmnt Γ

      data BStmnt Γ where
                      -- warning: scope in "ce" is interpreted in unusual way!
                      -- (TODO : consider speical type here)
        BS-let : (ce : ContextEntry) → {asn : PM  (AllowedScopeNarrowing Γ (scope ce) )}
                    → Expr (narrow Γ (scope ce) asn) (type ce) → BStmnt Γ    
        BS-publish! : (p : HonestParticipantId) → (PrivateSymbolOf Γ p)
                               → {_ : PM ( IsConsensus Γ ) } →  BStmnt Γ

      data NBStmnt Γ where
        NBS-require! : Expr Γ Bool → NBStmnt Γ
        NBS-deposit! : ParticipantId → Expr Γ Nat → NBStmnt Γ
        NBS-withdraw! : ParticipantId → Expr Γ Nat → NBStmnt Γ
        -- this is temporary solution, this constructors cannot apear in code, and are introduced on some passes, this distinction must be typesafe in the future!
        
        NBS-publishVal! : HonestParticipantId → Identifier → NBStmnt Γ

      data NBStmnt+Expr Γ where
        stmntNBS : NBStmnt Γ → {_ : PM ( IsConsensus Γ ) } →  NBStmnt+Expr Γ
        exprNBS : ∀ {Τ} → Expr Γ Τ → NBStmnt+Expr Γ

      Args Γ [] = Unit
      Args Γ (x ∷ []) = Arg Γ x
      Args Γ (x ∷ x₁ ∷ Τs) = Arg Γ x × Args Γ (x₁ ∷ Τs)

      data CtxChange (Γ : _) : Type₀ where
                      -- warning: scope in "ce" is interpreted in unusual way!
                      -- (TODO : consider speical type here)
        CC-extend : (ce : ContextEntry) → (asn : PM  (AllowedScopeNarrowing Γ (scope ce) ))  → CtxChange Γ    
        CC-makePublic : (p : HonestParticipantId) → (PrivateSymbolOf Γ p)
                               → (PM ( IsConsensus Γ ) ) → CtxChange Γ 

      -- postulate ctxChange : ∀ Γ → CtxChange Γ → Context
      -- ctxChange = {!!}

      bindingMechanics {Γ} (BS-let ce _) = ce ∷ Γ .entries
      bindingMechanics {Γ} (BS-publish! p x) = 
        map-ExistingFirstBy _ WitchIsAlso _ (Γ .entries) (toWitness' (psof-proof _ x)) 
           λ e _ _ → record e { scope = nothing }  

      -- redefine to be triviialy indepontent on scope' field
      bindingMechanics' Γ (bindingS x) = record Γ { entries =  bindingMechanics x } 
      bindingMechanics' Γ (nonBindingS x) = Γ

      Statements : Context → Type₀
      Statements Γ = Linked' bindingMechanics' Γ

      -- Expr-eq? : ∀ Γ Τ → (x y : Expr Γ Τ) → Dec (x ≡ y) 
      -- Expr-eq? Γ Τ (var (dsot x {x'})) (var (dsot y {y'})) = 
      --   dec-rec (x ≡ y)
      --     (λ p →   let q = True-Pa {A = λ i₁ → (fst (IsDefinedSymbolOfTy Γ Τ (p i₁)))} {(λ i₁ → (snd (IsDefinedSymbolOfTy Γ Τ (p i₁))))} {x'} {y'}
      --              in yes λ i → (var (dsot (p i) {{!!}})))
      --     {!!} 

      -- Expr-eq? Γ Τ (var x) (body x₁) = {!!}
      -- Expr-eq? Γ Τ (var x) (lit x₁) = {!!}
      -- Expr-eq? Γ Τ (body x) (var x₁) = {!!}
      -- Expr-eq? Γ Τ (body x) (body x₁) = {!!}
      -- Expr-eq? Γ Τ (body x) (lit x₁) = {!!}
      -- Expr-eq? Γ Τ (lit x) (var x₁) = {!!}
      -- Expr-eq? Γ Τ (lit x) (body x₁) = {!!}
      -- Expr-eq? Γ Τ (lit x) (lit x₁) = {!!}

      blankStmnt : ∀ {Γ} → Stmnt Γ
      blankStmnt = nonBindingS (exprNBS (lit tt))

      IsPureE : ∀ {Γ Τ} → Expr Γ Τ → DecPropΣ 

      IsPureS : ∀ {Γ} → Stmnt Γ → DecPropΣ

      IsPureStmnts : ∀ {Γ} → Statements Γ → DecPropΣ 


      IsPureE (var x) = Unit-dp
      IsPureE (_$'_ _ _) = Unit-dp
      IsPureE (body (bodyR stmnts₁ expr₁)) =
         (×-dp (IsPureStmnts stmnts₁) (IsPureE expr₁))
      IsPureE (lit x) = Unit-dp
      IsPureE (input x) = Empty-dp
      IsPureE (receivePublished _) = Empty-dp
      IsPureE (if x then x₁ else x₂) = ×-dp (IsPureE x) (×-dp (IsPureE x₁) (IsPureE x₂))
      IsPureE (sign q) = Unit-dp

      IsPureS (bindingS (BS-let ce x)) = (IsPureE x)
      IsPureS (bindingS (BS-publish! p x)) = Empty-dp
      IsPureS (nonBindingS (stmntNBS x)) = Empty-dp
      IsPureS (nonBindingS (exprNBS x)) = (IsPureE x)

      IsPureStmnts []L = Unit-dp
      IsPureStmnts (h ∷L x) = ×-dp (IsPureS h) (IsPureStmnts x)

      postulate isSet-Stmnt : (c : Context) → isSet (Stmnt c)


      STMNTS : Category ℓ-zero ℓ-zero
      STMNTS = FreeCategory' bindingMechanics' isSet-Context isSet-Stmnt

      module ToUnsafe where
        module U = Unsafe

        exprF : ∀ {Γ Τ} → Expr Γ Τ → U.Expr
        bodyF : ∀ {Γ Τ} → Body Γ Τ → U.Body
        stmntF : ∀ {Γ} → Stmnt Γ → U.Stmnt
        argsF : ∀ {Γ S} → Args Γ S → U.Args
        argF : ∀ {Γ Τ} → Arg Γ Τ → U.Arg  
       

        exprF {Τ = Τ} (var (dsot name₁)) = U.var Τ name₁
        exprF (body b) = (U.body (bodyF b))
        exprF (lit x) = U.lit (record { gType = _ ; gValue = x })
        exprF (bi' bIndex $' x₁) = bIndex U.$' argsF x₁
        exprF {Τ = Τ} (input x) = U.input Τ x
        exprF (sign x) = U.sign (argF x)
        exprF {Τ = Τ} (receivePublished p) = U.receivePublished Τ p
        exprF (if x then x₁ else x₂) = U.if exprF x then exprF x₁ else exprF x₂
        
        stmntF (bindingS (BS-let (ice scope₁ name₁ type₁) x)) = 
              U.bindingS (U.BS-let scope₁ name₁ type₁ (exprF x))

        stmntF {Γ} (bindingS (BS-publish! p x@(psof name₂ {y}))) = 
               U.bindingS (U.BS-publish! p (IsPrivateSymbolOf→GType Γ p _ (toWitness' y) ) name₂) 
        stmntF (nonBindingS (stmntNBS x)) = (U.nonBindingS (U.stmntNBS (h x)))
           where
             h : NBStmnt _ → U.NBStmnt
             h (NBS-require! x) = U.NBS-require! (exprF x)
             h (NBS-deposit! p x₁) = U.NBS-deposit! p (exprF x₁)
             h (NBS-withdraw! p x₁) = U.NBS-withdraw! p (exprF x₁)
             h (NBS-publishVal! p x₁) = U.NBS-publishVal! p x₁
             
        stmntF (nonBindingS (exprNBS x)) = (U.nonBindingS (U.exprNBS (exprF x)))

        bodyF (bodyR []L expr₁) = U.bodyR [] (exprF expr₁)
        bodyF (bodyR (h ∷L stmnts₁) expr₁) =
           let U.bodyR xs e = bodyF (bodyR (stmnts₁) expr₁)
           in  U.bodyR (stmntF h ∷ xs) e 

        argF (var-a (dsot name₁)) = U.var-a name₁
        argF (lit-a x) = U.lit-a (record { gType = _ ; gValue = x })
        
        argsF {S = []} x = []
        argsF {S = _ ∷ []} x = [ argF x ]
        argsF {S = _ ∷ _ ∷ _} x = argF (proj₁ x) ∷ argsF (proj₂ x)

        stmntsF : ∀ {Γ} → Statements Γ → List U.Stmnt
        stmntsF []L = []
        stmntsF (h ∷L x) = stmntF h ∷ stmntsF x

 
    toParamValue : ∀ (l : List IdentifierWithType)  → ParametersValue l →
                   ∀ Τ s → 
                   IsMemberOf (iwt s Τ) l →
                   GTypeAgdaRep Τ
    toParamValue (x₂ ∷ l) (x , xs) Τ s (inl p) = subst-GTypeAgdaRep (cong type (sym p)) x -- 
    toParamValue (x₂ ∷ l) (x , xs) Τ s (inr (_ , x₁)) = (toParamValue l xs Τ s x₁) --

    InteractionHead≡ : {ih₀ ih₁ : InteractionHead}
                          → InteractionHead.participantsWM ih₀ ≡ InteractionHead.participantsWM ih₁ 
                          → InteractionHead.parameters ih₀ ≡ InteractionHead.parameters ih₁
                          → ih₀ ≡ ih₁
    InteractionHead.participantsWM (InteractionHead≡ x x₁ i) = x i
    InteractionHead.parameters (InteractionHead≡ x x₁ i) = x₁ i
    InteractionHead.uniqueParams (InteractionHead≡ {ih₀} {ih₁} x x₁ i) =
       isOfHLevel→isOfHLevelDep 1 (λ _ → isProp-PM) (InteractionHead.uniqueParams ih₀) (InteractionHead.uniqueParams ih₁) x₁ i
    InteractionHead.uniquePtcpnts (InteractionHead≡ {ih₀} {ih₁} x x₁ i) =
       isOfHLevel→isOfHLevelDep 1 (λ _ → isProp-PM) (InteractionHead.uniquePtcpnts ih₀) (InteractionHead.uniquePtcpnts ih₁) x i

    fixProofs : ∀ {ℓ} {ih₀ ih₁ : InteractionHead}
                → (A : InteractionHead → Type ℓ)
                → _ → _
                → A ih₀ → A ih₁
    fixProofs A x y = subst A (InteractionHead≡ x y)

    fixProofs' : ∀ {ℓ} {ptps : _} {params : _} → 
                       ∀ {p₀ p₁ p₂ p₃}
                → (A : InteractionHead → Type ℓ)
                → A (interactionHead ptps params {p₀} {p₁})
                → A (interactionHead ptps params {p₂} {p₃})
    fixProofs' A = subst A (InteractionHead≡ refl refl)


    record Interaction : Type₀ where
      -- pattern
      constructor interaction
      field
        head : InteractionHead

      open InteractionHead head public

      field
        code : Linked' bindingMechanics' emptyContext 

    open InteractionHead public

    -- open Interaction public

    infixl 6 interaction⟨_,_⟩_
    infixr 50 _∶_ 

    infixr 10 _;b_
    infixr 15 _;_
    infix 17 _;₁
    infixr 15 _;'_

    infix 30 set_∶_≔_
    infix 30 at_set_∶_≔_


    infix 60 <_>

    infix 40 _$_


    pattern interaction⟨_,_⟩_ prts prms stmnts = interaction (interactionHead prts prms ) stmnts

    pattern _$_ x y =  bi' x $' y


    pattern _∶_ x y = iwt x y 

    pattern _;_ x y = _∷L_ x y


    pattern _;₁ x = x ; []L  

    pattern _;'_ x y = x ; y ;₁  

    pattern set_∶_≔_ x y z =
      bindingS (BS-let (ice nothing x y) z)

    pattern at_set_∶_≔_ p x y z =
      bindingS
         (BS-let (ice (just (pId p)) x y) z)

    pattern publish!_⟶_ x y = bindingS (BS-publish! (pId x) (psof y))

    pattern deposit!_⟶_ x y = nonBindingS (stmntNBS (NBS-deposit! (pId x) y))

    pattern withdraw!_⟵_ x y = nonBindingS (stmntNBS (NBS-withdraw! (pId x) y))

    pattern require!_ x = nonBindingS (stmntNBS (NBS-require! x))


    <_> : ∀ {IH Γ} → {A : Type₀} → ⦃ isGlowTy : IsGlowTy A ⦄ →
             A →  Expr IH Γ (IsGlowTy.glowRep isGlowTy)
    <_> {IH} {Γ} {A} ⦃ isGlowTy ⦄ x = lit (IsGlowTy.cast isGlowTy x)

    pattern _;b_ x y = body (bodyR x y)  

    infixr 60 v_

    pattern v_ x = var (dsot x)
    pattern va_ x = var-a (dsot x)

  open AST

  toProofs : Interaction zero  →
                Interaction one
  toProofs = transport (λ i → Interaction (seg i))

  toProofsE : ∀ {Τ} → Σ[ ih ∈ InteractionHead zero ] Σ[ Γ ∈ _ ] Expr {prop-mode = zero} ih Γ Τ
                    → Σ[ ih ∈ InteractionHead one ] Σ[ Γ ∈ _ ] Expr {prop-mode = one} ih Γ Τ 
  toProofsE {Τ} = transport (λ i → Σ[ ih ∈ InteractionHead (seg i) ] Σ[ Γ ∈ _ ] Expr {prop-mode = (seg i)} ih Γ Τ)


  fromProofs : Interaction one  →
                Interaction zero
  fromProofs = transport (λ i → Interaction (seg (~ i)))

  


    







{-# OPTIONS --cubical  #-}
module Glow.Simple.ParamsSubst where

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

open import Glow.Simple.AST

module _ where


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


  stripParamsPrivateSymbolIsDefinedVariableOfTy-lemma : ∀ {ih : _} → (Γ : Context ih) → ∀ Τ s
           → IsDefinedVariableOfTyTest Γ Τ s ≡ IsDefinedVariableOfTyTest (stripParamsCtx Γ) Τ s 
  stripParamsPrivateSymbolIsDefinedVariableOfTy-lemma (con [] scope'') Τ s = refl
  stripParamsPrivateSymbolIsDefinedVariableOfTy-lemma {ih} (con (x ∷ entries₁) scope'') Τ s =
    𝟚-elim
      {A = λ bb
        → _≡_ {ℓ-zero} {𝟚}
      (recMaybe {ℓ-zero} {𝟚} {ℓ-zero} {ContextEntry ih} false
       (λ y →
          _canAccessTest_ {ih} (con (x ∷ entries₁) scope'')
          scope'' (scope y)
          and GTy== (type y) Τ)
       (if_then_else_ {ℓ-zero} {Maybe {ℓ-zero} (ContextEntry ih)}
        bb (just x)
        (findBy {ℓ-zero} {ContextEntry ih}
         (λ x₁ → primStringEquality s (name x₁)) entries₁)))
      (recMaybe {ℓ-zero} {𝟚} {ℓ-zero}
       {ContextEntry (interactionHead (participants ih) [] {tt*})} false
       (λ y →
          _canAccessTest_ {interactionHead (participants ih) [] {tt*}}
          (con
           (ice
            (map-Maybe {ℓ-zero} {ParticipantId ih} {ℓ-zero}
             {ParticipantId (interactionHead (participants ih) [] {tt*})}
             (λ x₁ → pId (x₁ .name) {x₁ .isIn}) (x .scope))
            (x .name) (x .type)
            ∷
            map-List {ℓ-zero} {ContextEntry ih} {ℓ-zero}
            {ContextEntry (interactionHead (participants ih) [] {tt*})}
            (λ x₁ →
               ice
               (map-Maybe {ℓ-zero} {ParticipantId ih} {ℓ-zero}
                {ParticipantId (interactionHead (participants ih) [] {tt*})}
                (λ x₁ → pId (x₁ .name) {x₁ .isIn}) (x₁ .scope))
               (x₁ .name) (x₁ .type))
            entries₁)
           (map-Maybe {ℓ-zero} {ParticipantId ih} {ℓ-zero}
            {ParticipantId (interactionHead (participants ih) [] {tt*})}
            (λ x₁ → pId (x₁ .name) {x₁ .isIn}) scope''))
          (map-Maybe {ℓ-zero} {ParticipantId ih} {ℓ-zero}
           {ParticipantId (interactionHead (participants ih) [] {tt*})}
           (λ x₁ → pId (x₁ .name) {x₁ .isIn}) scope'')
          (scope y)
          and GTy== (type y) Τ)
       (if_then_else_ {ℓ-zero}
        {Maybe {ℓ-zero}
         (ContextEntry (interactionHead (participants ih) [] {tt*}))}
        bb
        (just
         (ice
          (map-Maybe {ℓ-zero} {ParticipantId ih} {ℓ-zero}
           {ParticipantId (interactionHead (participants ih) [] {tt*})}
           (λ x₁ → pId (x₁ .name) {x₁ .isIn}) (x .scope))
          (x .name) (x .type)))
        (findBy {ℓ-zero}
         {ContextEntry (interactionHead (participants ih) [] {tt*})}
         (λ x₁ → primStringEquality s (name x₁))
         (map-List {ℓ-zero} {ContextEntry ih} {ℓ-zero}
          {ContextEntry (interactionHead (participants ih) [] {tt*})}
          (λ x₁ →
             ice
             (map-Maybe {ℓ-zero} {ParticipantId ih} {ℓ-zero}
              {ParticipantId (interactionHead (participants ih) [] {tt*})}
              (λ x₁ → pId (x₁ .name) {x₁ .isIn}) (x₁ .scope))
             (x₁ .name) (x₁ .type))
          entries₁))))}
      (stripParamsPrivateSymbolIsDefinedVariableOfTy-lemma {ih} (con (entries₁) scope'') Τ s)
      (cong (_and GTy== (type x) Τ)
        let tyElim = λ (mb1 : Maybe (ParticipantId ih)) (mb : Scope ih) → 
                (con (x ∷ entries₁) mb canAccessTest
               mb) mb1

              ≡
              (con
               (ice
                (map-Maybe (λ x₁ → pId (x₁ .name) {x₁ .isIn}) mb1)
                (x .name) (x .type)
                ∷
                map-List
                (λ x₁ →
                   ice
                   (map-Maybe (λ x₂ → pId (x₂ .name) {x₂ .isIn}) (x₁ .scope))
                   (x₁ .name) (x₁ .type))
                entries₁)
               (map-Maybe (λ x₁ → pId (x₁ .name) {x₁ .isIn}) mb)
               canAccessTest
               map-Maybe (λ x₁ → pId (x₁ .name) {x₁ .isIn}) mb)
              (map-Maybe (λ x₁ → pId (x₁ .name) {x₁ .isIn}) mb1)
        in
         (maybe-elim
          {B = tyElim (x .scope)
              }
           (maybe-elim {B = λ x₁ → tyElim x₁ nothing} refl (λ _ → refl) (x .scope))
           (λ q → maybe-elim {B = λ x₁ → tyElim x₁ (just q)} refl (λ _ → refl) (x .scope))
           scope''))
      (primStringEquality s (name x))



  stripParamsPrivateSymbolIsDefinedVariableOfTy : ∀ {ih : _} → (Γ : Context ih) → ∀ Τ s
           → ⟨ Bool→Type' (IsDefinedVariableOfTyTest Γ Τ s) ⟩
           → ⟨ Bool→Type' (IsDefinedVariableOfTyTest (stripParamsCtx Γ) Τ s) ⟩
           
  stripParamsPrivateSymbolIsDefinedVariableOfTy Γ Τ s = subst (typ ∘ Bool→Type') (stripParamsPrivateSymbolIsDefinedVariableOfTy-lemma Γ Τ s)
    



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
                   ∀ {Γ : _} →  Statements _ Γ → Statements _ (stripParamsCtx Γ) 

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

      h-expr : {Γ : Context ih} → ∀ {Τ}
             → (b : Expr ih Γ Τ) → Expr _ (stripParamsCtx Γ) Τ



      h  (bindingS x) = bindingS (BS-lemma x)
         where
              BS-lemma : {Γ : Context ih} →  BStmnt ih Γ -> BStmnt _ _
              BS-lemma {Γ} (BS-let x {asn} y) =
                                 BS-let (ice (map-Maybe (λ x → pId (x .name) {x .isIn}) (x .scope)) (x .name) (x .type))
                                      {h-narrowing x asn} (subst (λ x₁ → Expr (stripParamsHead ih) x₁ (type x)) (narrow-stripsParams-comm Γ x asn) (h-expr y))
                 where
                       h-narrowing : {Γ : Context ih} (x : ContextEntry ih) (asn : ⟨ (AllowedScopeNarrowing Γ (scope x))⟩ )
                              → ⟨ (AllowedScopeNarrowing
                                   (con (map-List stripCtxEntry (entries Γ))
                                    (map-Maybe partIdSubst (scope' Γ)))
                                   (map-Maybe (λ x₁ → pId (x₁ .name) {x₁ .isIn}) (x .scope)))⟩
                       h-narrowing {con entries₁ nothing} (ice nothing name₁ type₁) asn = _
                       h-narrowing {con entries₁ nothing} (ice (just x) name₁ type₁) asn = _
                       h-narrowing {con entries₁ (just x)} (ice nothing name₁ type₁) asn = _

                       narrow-stripsParams-comm : (Γ : Context ih) (x : ContextEntry ih)
                                              (asn : ⟨ (AllowedScopeNarrowing Γ (scope x))⟩)  →
                                            stripParamsCtx (narrow ih Γ (scope x) asn) ≡
                                            narrow (interactionHead (participants ih) [])
                                            (con (map-List stripCtxEntry (entries Γ))
                                             (map-Maybe partIdSubst (scope' Γ)))
                                            (map-Maybe (λ x₁ → pId (x₁ .name)) (x .scope))
                                            (h-narrowing x asn)
                       narrow-stripsParams-comm (con entries₁ nothing) (ice nothing name₁ type₁) asn = refl
                       narrow-stripsParams-comm (con entries₁ nothing) (ice (just x) name₁ type₁) asn = refl
                       narrow-stripsParams-comm (con entries₁ (just x)) (ice nothing name₁ type₁) asn = refl


              BS-lemma {Γ} (BS-publish! p x {y}) = BS-publish! (partIdSubst p) (stripParamsPrivateSymbol x) {stripParamsCtxConsensus {ih} {Γ} y}


      h {Γ} (nonBindingS x) = nonBindingS (z x)
         where

           zz : NBStmnt _ _ → NBStmnt _ _ 
           zz (NBS-require! x) = NBS-require! (h-expr x)
           zz (NBS-deposit! p {y} x) = NBS-deposit! (partIdSubst p) {stripParamsCtxConsensus {ih} {Γ}  y} (h-expr x)
           zz (NBS-withdraw! p {y} x) = NBS-withdraw! (partIdSubst p) {stripParamsCtxConsensus {ih} {Γ} y} (h-expr x)

           z : NBStmnt+Expr ih _ → NBStmnt+Expr (stripParamsHead ih) _
           z (stmntNBS x) =  stmntNBS (zz x)
           z (exprNBS x) = exprNBS (h-expr x)




      h-expr {Γ} {Τ} (var x) = 
              Cubical.Data.Sum.elim
                (λ a → var (dsot (x .name) {stripParamsPrivateSymbolIsDefinedVariableOfTy Γ Τ (x .name) a }))
                (lit ∘ toParamValue vv {Γ} Τ (x .name))
                (proj₁ (DefinedSymbolOfTy-case Γ (name x)) (x .isDefinedSymbolOfTy))
              
      h-expr (stmnts₁ ;b x) =  paramSubst vv stmnts₁ ;b subst (λ x₁ → Expr _ x₁ _) (wwww stmnts₁ ) (h-expr x)
      h-expr (lit x) = lit x

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
          qqq (ice nothing name₁ type₁) =
            subst (λ bb →
                            _≡_ {ℓ-zero}
                      {ContextEntry (interactionHead (participants ih) [] {tt*})}
                      (ice
                       (map-Maybe {ℓ-zero} {ParticipantId ih} {ℓ-zero}
                        {ParticipantId (interactionHead (participants ih) [] {tt*})}
                        (λ x₁ → pId (name x₁))
                        (if_then_else_ {ℓ-zero} {ContextEntry ih}
                         bb
                         (record { scope = nothing ; name = name₁ ; type = type₁ })
                         (ice nothing name₁ type₁) .scope))
                       (if_then_else_ {ℓ-zero} {ContextEntry ih}
                        bb
                        (record { scope = nothing ; name = name₁ ; type = type₁ })
                        (ice nothing name₁ type₁) .name)
                       (if_then_else_ {ℓ-zero} {ContextEntry ih}
                        bb
                        (record { scope = nothing ; name = name₁ ; type = type₁ })
                        (ice nothing name₁ type₁) .type))
                      (if_then_else_ {ℓ-zero}
                       {ContextEntry (interactionHead (participants ih) [] {tt*})}
                       bb -- (false and primStringEquality name₁ (x .name))
                       (record { scope = nothing ; name = name₁ ; type = type₁ })
                       (ice nothing name₁ type₁)))
                  
                  (sym (F-and _))
                  refl
          
          qqq (ice (just xx) name₁ type₁) = 
            𝟚-elim {A = λ bb →              
                      _≡_ {ℓ-zero}
                         {ContextEntry (interactionHead (participants ih) [] {tt*})}
                         (ice
                          (map-Maybe {ℓ-zero} {ParticipantId ih} {ℓ-zero}
                           {ParticipantId (interactionHead (participants ih) [] {tt*})}
                           (λ x₁ → pId (name x₁))
                           (if_then_else_ {ℓ-zero} {ContextEntry ih} bb
                            (record { scope = nothing ; name = name₁ ; type = type₁ })
                            (ice (just xx) name₁ type₁) .scope))
                          (if_then_else_ {ℓ-zero} {ContextEntry ih} bb
                           (record { scope = nothing ; name = name₁ ; type = type₁ })
                           (ice (just xx) name₁ type₁) .name)
                          (if_then_else_ {ℓ-zero} {ContextEntry ih} bb
                           (record { scope = nothing ; name = name₁ ; type = type₁ })
                           (ice (just xx) name₁ type₁) .type))
                         (if_then_else_ {ℓ-zero}
                          {ContextEntry (interactionHead (participants ih) [] {tt*})} bb
                          (record { scope = nothing ; name = name₁ ; type = type₁ })
                          (ice (just (pId (name xx))) name₁ type₁))}
               refl
               refl
               (primStringEquality (name p) (name xx) and
                             primStringEquality name₁ (name x))

      hh {x = nonBindingS x} = refl

  wwww {ih} = map-Linked'-map-fold (stripParamsCtx {ih}) _ _ 

  param-sub-test : ℕ × 𝟚 × 𝟚 × Unit → Linked'
                                        (bindingMechanics'
                                         (stripParamsHead
                                          (interactionHead ("A" ∷ "B" ∷ [])
                                           ("pI1" ∶ Nat ∷ "b2" ∶ Bool ∷ "b1" ∶ Bool ∷ []))))
                                        (stripParamsCtx (Interaction.emptyContext someInteraction))
  param-sub-test x = paramSubst x (Interaction.code someInteraction)

  zzz : Type₀
  zzz =
    let q : ℕ × 𝟚 × 𝟚 × Unit
        q = 3 , false , true , _
    in param-sub-test q ≡ (
        set "x" ∶ Bool ≔ < true > ;
        at "B" set "y" ∶ Bool ≔ < true > ;
        at "A" set "xx" ∶ Bool ≔ (
            require! < false > ;'
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

  zzz' : zzz
  zzz' = refl

  zzz2 : Type₀
  zzz2 =
    ∀ x y z → param-sub-test (x , y , z , _) ≡ (
        set "x" ∶ Bool ≔ < true > ;
        at "B" set "y" ∶ Bool ≔ < z > ;
        at "A" set "xx" ∶ Bool ≔ (
            require! < y > ;'
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

  zzz2' : zzz2
  zzz2' _ _ _ = refl

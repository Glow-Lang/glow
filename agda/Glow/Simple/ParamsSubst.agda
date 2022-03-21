
{-# OPTIONS --cubical  #-}
module Glow.Simple.ParamsSubst where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool hiding (if_then_else_) renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.Simple.AST

open import Glow.DecEqMore

open import Cubical.HITs.Interval

module ParamsSubst {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
              {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where

  prop-mode = one
  
  open AST Identifier builtIns prop-mode

  open PropMode prop-mode 

  -- open InteractionHead

  stripParamsHead : InteractionHead → InteractionHead 
  stripParamsHead ih = interactionHead (participantsWM ih) [] {_} {uniquePtcpnts ih}


  stripParamsCtx : ∀ {ih : _} → Context ih → Context (stripParamsHead ih)
  stripParamsCtx Γ = con (Γ .entries) (Γ .scope')

  -- TODO : remove unsafe pragma by stratification on nesting depth
  {-# TERMINATING #-}
  paramSubst : ∀ {ih : _} → ParametersValue (parameters ih) → 
                   ∀ {Γ : _} →  Statements _ Γ → Statements _ (stripParamsCtx Γ) 



  paramSubst {ih}  vv = map-Linked'-map _ h hh
    where



      h : {Γ : Context ih}
             → (b : Stmnt ih Γ) → Stmnt _ (stripParamsCtx Γ)

      h-expr : {Γ : Context ih} → ∀ {Τ}
             → (b : Expr ih Γ Τ) → Expr _ (stripParamsCtx Γ) Τ

      h-arg : ∀ {Γ Τ} → Arg ih Γ Τ → Arg _ (stripParamsCtx Γ) Τ
      h-arg (AST.var-a (AST.dsot x {y})) =
          sum-elim
           (λ a → var-a (dsot x {inl a}))
           (lit-a ∘ (lookup-ParametersValue (ih .parameters) vv (iwt x _)) ∘ proj₂)
            y
      h-arg (AST.lit-a x) = (AST.lit-a x)


      h-args : ∀ {Γ Τs}  → Args ih Γ Τs → Args _ (stripParamsCtx Γ) Τs
      h-args {Τs = []} x = tt
      h-args {Τs = x₁ ∷ []} x = h-arg x
      h-args {Τs = x₁ ∷ x₂ ∷ Τs} (x , x₃) = h-arg x , h-args  x₃ 
      

      h  (bindingS x) = bindingS (BS-lemma x)
         where
              BS-lemma : {Γ : Context ih} →  BStmnt ih Γ -> BStmnt _ (stripParamsCtx Γ)
              BS-lemma (BS-let x {asn} y) = (BS-let x {asn} (h-expr y))  
              BS-lemma (BS-publish! p (psof name₁ {w}) {y}) = (BS-publish! p (psof name₁ {w}) {y})


      h (nonBindingS x) = nonBindingS (z x)
         where

           zz : NBStmnt _ _ → NBStmnt _ _ 
           zz (NBS-require! x) = NBS-require! (h-expr x)
           zz (NBS-deposit! p x) = NBS-deposit! p (h-expr x)
           zz (NBS-withdraw! p x) = NBS-withdraw! p (h-expr x)
           zz (NBS-publishVal! x y) = NBS-publishVal! x y

           z : NBStmnt+Expr ih _ → NBStmnt+Expr (stripParamsHead ih) _
           z (stmntNBS x {q}) =  stmntNBS (zz x) {q}
           z (exprNBS x) = exprNBS (h-expr x)

      h-expr (var (dsot x {y})) =
         sum-elim
           (λ a → var (dsot x {inl a}))
           (lit ∘ (lookup-ParametersValue (ih .parameters) vv (iwt x _)) ∘ proj₂)
            y



      h-expr (stmnts₁ ;b x) =
         paramSubst {ih = ih} vv stmnts₁ ;b subst (λ x₁ → Expr _ x₁ _)
             -- TODO : improve evaluation performance by introducing specialized "subst"
             -- specialisation should be not only on Expr, but also on map-Linked'-map-fold
        (map-Linked'-map-fold (stripParamsCtx {ih}) _ _ stmnts₁ ) (h-expr x)
      h-expr (lit x) = lit x
      h-expr (_$'_ f xs) = _$'_ f (h-args xs)

      h-expr (input msg {y}) = input msg {y}
      -- h-expr (receivePublished x {y}) = publishVal x {y}
      h-expr (if b then t else f) = if (h-expr b) then (h-expr t) else (h-expr f)
      h-expr (AST.sign q {y} {w}) = (AST.sign (h-arg q) {y} {w})
      h-expr (AST.receivePublished x {y}) = AST.receivePublished x {y}

      hh : (Γ : Context ih) (x : Stmnt _ Γ) →
         stripParamsCtx (bindingMechanics' _ Γ x) ≡
         bindingMechanics' (interactionHead (participantsWM ih) [])
         (stripParamsCtx Γ) (h x)
      hh _ (bindingS (BS-let _ _)) = refl 
      hh _ (AST.bindingS (AST.BS-publish! _ (AST.psof name₁))) = refl
      hh _ (nonBindingS _) = refl

      -- h-args = ?

-- module Test-String where
--   open AST String {{String-Discrete-postulated}} zero

--   module ParamsSubstS = ParamsSubst {{String-Discrete-postulated}}

--   someInteraction : Interaction 
--   someInteraction =  
--        interaction⟨   "A" ∷ "B" ∷ [] ,  "pI1" ∶ Nat ∷ "b2" ∶ Bool ∷ "b1" ∶ Bool ∷ [] ⟩ (
--             set "x" ∶ Bool ≔ < true > ;
--             at "B" set "y" ∶ Bool ≔ v "b1" ;
--             at "A" set "xx" ∶ Bool ≔
--              ( if v "b1"
--                then
--                   (
--                   set "z" ∶ Bool ≔ input "enter choice 1" ;₁ ;b
--                   v "z"
--                 )
--                else (
--                 require! v "b2" ;'
--                 -- publish! "B" ⟶ "y" ;
--                 -- withdraw! "B" ⟵ < 3 > ;
--                 -- deposit! "B" ⟶ < 2 > ;
--                 set "z" ∶ Bool ≔ < false > ;b
--                 < true >
--                 )) ;
--             deposit! "B" ⟶ < 2 > ;
--             at "A" set "yq" ∶ Bool ≔ input "enter choice 2" ;
--             withdraw! "B" ⟵ < 3 > ;
--             publish! "A" ⟶ "xx" ;        

--             publish! "B" ⟶ "y" ;'        
--             set "yy" ∶ Bool ≔ v "y" )


--   param-sub-test : ℕ × 𝟚 × 𝟚 × Unit → Linked'
--                                         (bindingMechanics'
--                                          (ParamsSubstS.stripParamsHead
--                                           (interactionHead ("A" ∷ "B" ∷ [])
--                                            ("pI1" ∶ Nat ∷ "b2" ∶ Bool ∷ "b1" ∶ Bool ∷ []))))
--                                         (ParamsSubstS.stripParamsCtx (Interaction.emptyContext someInteraction))
--   param-sub-test vv = ParamsSubstS.paramSubst vv (Interaction.code someInteraction)
--       -- {!ParamsSubstS.paramSubst vv (Interaction.code someInteraction)!}


--   zzz :
--     let q : ℕ × 𝟚 × 𝟚 × Unit
--         q = 3 , false , true , _
--         bT : Statements _ _
--         bT = (
--           set "x" ∶ Bool ≔ < true > ;
--           at "B" set "y" ∶ Bool ≔ < true > ;
--           at "A" set "xx" ∶ Bool ≔ (
--               require! < false > ;'
--               -- publish! "B" ⟶ "y" ;
--               -- withdraw! "B" ⟵ < 3 > ;
--               -- deposit! "B" ⟶ < 2 > ;
--               set "z" ∶ Bool ≔ < false > ;b
--               < true >
--               );
--           deposit! "B" ⟶ < 2 > ;
--           withdraw! "B" ⟵ < 3 > ;
--           publish! "B" ⟶ "y" ;'        
--           set "yy" ∶ Bool ≔ v "y"
--           )
--     in bT ≡ param-sub-test q 

--   zzz = refl



-- module Test-ℕ where
--   open AST ℕ 

--   module ParamsSubstS = ParamsSubst {ℕ}

--   someInteraction : Interaction
--   someInteraction =  
--      interaction⟨   1 ∷ 2 ∷ [] ,  3 ∶ Nat ∷ 4 ∶ Bool ∷ 5 ∶ Bool ∷ [] ⟩ (
--           set 6 ∶ Bool ≔ < true > ;
--           at 2 set 7 ∶ Bool ≔ v 5 ;
--           at 1 set 8 ∶ Bool ≔ (
--               require! v 4 ;'
--               -- publish! "B" ⟶ "y" ;
--               -- withdraw! "B" ⟵ < 3 > ;
--               -- deposit! "B" ⟶ < 2 > ;
--               set 9 ∶ Bool ≔ < false > ;b
--               < true >
--               );
--           deposit! 2 ⟶ < 2 > ;
--           withdraw! 2 ⟵ < 3 > ;
--           publish! 2 ⟶ 7 ;'        
--           set 10 ∶ Bool ≔ v 7 )


--   param-sub-test : ℕ × 𝟚 × 𝟚 × Unit → Linked'
--                                         (bindingMechanics'
--                                          (ParamsSubstS.stripParamsHead
--                                           (interactionHead (1 ∷ 2 ∷ [])
--                                            (3 ∶ Nat ∷ 4 ∶ Bool ∷ 5 ∶ Bool ∷ []))))
--                                         (ParamsSubstS.stripParamsCtx (Interaction.emptyContext someInteraction))
--   param-sub-test vv = ParamsSubstS.paramSubst vv (Interaction.code someInteraction)

--   zzz-0 : Linked'
--             (bindingMechanics'
--              (ParamsSubstS.stripParamsHead
--               (interactionHead (1 ∷ 2 ∷ [])
--                (3 ∶ Nat ∷ 4 ∶ Bool ∷ 5 ∶ Bool ∷ []))))
--             (ParamsSubst.stripParamsCtx
--              (Interaction.emptyContext someInteraction))
              
--   zzz-0 = param-sub-test (3 , false , true , _)
--            -- bindingS
--             -- (BS-let (AST.ice nothing 6 Bool) {_}
--             --  (lit true))
--             -- ∷L
--             -- (bindingS
--             --  (BS-let
--             --   (transp {λ i → ℓ-zero}
--             --    (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --    (AST.ice (just (AST.pId 2 {_})) 7 Bool))
--             --   {_} (lit true))
--             --  ∷L
--             --  (bindingS
--             --   (BS-let
--             --    (transp {λ i → ℓ-zero}
--             --     (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --     (transp {λ i → ℓ-zero}
--             --      (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --      (AST.ice (just (AST.pId 1 {_})) 8 Bool)))
--             --    {_}
--             --    (body
--             --     (transp {λ i → ℓ-zero}
--             --      (λ i →
--             --         AST.Body {ℕ} ⦃ ℕ-Discrete ⦄
--             --         (AST.interactionHead (1 ∷ 2 ∷ []) [] {_})
--             --         (record
--             --          { entries =
--             --              transp {λ i₁ → ℓ-zero}
--             --              (λ i₁ → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) (~ i)
--             --              (AST.ice (just (AST.pId 2 {_})) 7 Bool)
--             --              ∷ AST.ice nothing 6 Bool ∷ []
--             --          ; scope' = just (AST.pId 1 {_})
--             --          })
--             --         Bool)
--             --      i0
--             --      (transp {λ i → ℓ-zero}
--             --       (λ i →
--             --          AST.Body {ℕ} ⦃ ℕ-Discrete ⦄
--             --          (AST.interactionHead (1 ∷ 2 ∷ []) [] {_})
--             --          (record
--             --           { entries =
--             --               AST.ice (just (AST.pId 2 {_})) 7 Bool ∷ AST.ice nothing 6 Bool ∷ []
--             --           ; scope' = just (AST.pId 1 {_})
--             --           })
--             --          Bool)
--             --       i0
--             --       (bodyR
--             --        (nonBindingS
--             --         (stmntNBS
--             --          (NBS-require! (lit false)))
--             --         ∷L
--             --         (bindingS
--             --          (BS-let
--             --           (transp {λ i → ℓ-zero}
--             --            (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --            (AST.ice nothing 9 Bool))
--             --           {_} (lit false))
--             --          ∷L []L))
--             --        (lit true))))))
--             --   ∷L
--             --   (nonBindingS
--             --    (stmntNBS
--             --     (NBS-deposit! (AST.pId 2 {_}) {_}
--             --      (lit 2)))
--             --    ∷L
--             --    (nonBindingS
--             --     (stmntNBS
--             --      (NBS-withdraw! (AST.pId 2 {_}) {_}
--             --       (lit 3)))
--             --     ∷L
--             --     (bindingS
--             --      (BS-publish! (AST.pId 2 {_})
--             --       (psof 7 {_}) {_})
--             --      ∷L
--             --      (bindingS
--             --       (BS-let
--             --        (transp {λ i → ℓ-zero}
--             --         (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --         (transp {λ i → ℓ-zero}
--             --          (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --          (transp {λ i → ℓ-zero}
--             --           (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --           (transp {λ i → ℓ-zero}
--             --            (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --            (transp {λ i → ℓ-zero}
--             --             (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --             (transp {λ i → ℓ-zero}
--             --              (λ i → AST.ContextEntry' ℕ ⦃ ℕ-Discrete ⦄ {1 ∷ 2 ∷ []}) i0
--             --              (AST.ice nothing 10 Bool)))))))
--             --        {_} (var (dsot 7 {_})))
--             --       ∷L []L))))))

--   zzz :
--     let q : ℕ × 𝟚 × 𝟚 × Unit
--         q = 3 , false , true , _
--     in (
--           set 6 ∶ Bool ≔ < true > ;
--           at 2 set 7 ∶ Bool ≔ < true > ;
--           at 1 set 8 ∶ Bool ≔ (
--               require! < false > ;'
--               -- publish! "B" ⟶ "y" ;
--               -- withdraw! "B" ⟵ < 3 > ;
--               -- deposit! "B" ⟶ < 2 > ;
--               set 9 ∶ Bool ≔ < false > ;b
--               < true >
--               );
--           deposit! 2 ⟶ < 2 > ;
--           withdraw! 2 ⟵ < 3 > ;
--           publish! 2 ⟶ 7 ;'        
--           set 10 ∶ Bool ≔ v 7 ) ≡ param-sub-test q

--   zzz = refl

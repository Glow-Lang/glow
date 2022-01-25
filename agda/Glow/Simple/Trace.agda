
{-# OPTIONS --cubical  #-}
module Glow.Simple.Trace where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool hiding (if_then_else_)  renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.Simple.AST

open import Glow.DecEqMore

open import Glow.Simple.ContextMore

open import Glow.Simple.VarSubst

-- open import Glow.Simple.Monad


open import Cubical.HITs.Interval

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}} where


  module Trace {ptps : List Identifier}  where

    open AST.InteractionHead  {prop-mode = one} (AST.interactionHead ptps []) 

    open SubstAll {Identifier} {ptps}
    open SubstOne {Identifier} {ptps}


    {-# TERMINATING #-}
    Trace : ∀ sc → Statements (con [] sc) → Type₀
    TraceB : ∀ sc {Τ} → (s : Stmnt (con [] sc))
                      → (bd : Body (bindingMechanics' _ s ) Τ)
                      → IsEmpty ⟨ IsPureE (body (AST.bodyR (s ∷L (stmnts bd)) (expr bd))) ⟩ 
                    → Σ Type₀ λ Tr →  (Tr → (Maybe (GTypeAgdaRep Τ)))
        
    TraceNBS : ∀ sc → NBStmnt (con [] sc) →
       Σ (Σ Type₀ λ Ty → Ty → Type₀ → Type₀)
          λ x → ∀ {Ty'} → {y : (fst x)} →  ((λ b → b Ty') ∘ (snd x)) y
                           → Maybe Ty' 

    TraceE : ∀ sc → ∀ {Τ} → (e : Expr (con [] sc) Τ) → IsEmpty ⟨ IsPureE e ⟩
                                  → Σ Type₀ λ Tr → (Tr → (Maybe (GTypeAgdaRep Τ)))

    TraceE' : ∀ sc → ∀ {Τ} → (e : Expr (con [] sc) Τ) 
                                  → Σ Type₀ λ Tr → (Tr → (Maybe (GTypeAgdaRep Τ)))

    Trace sc []L = Unit
    Trace sc (h ∷L x) with proj₁ (snd (IsPureS h))
    ... | no ¬p = fst (TraceB sc h (AST.bodyR x (lit tt)) (¬p ∘ proj₁ ∘ proj₁))
    ... | yes p with h
    ... | AST.bindingS (AST.BS-let ce x₁) =
             Trace sc (substOneStmnts (inl (evalPureExpr x₁ p)) (mkStatements* x))
    ... | AST.nonBindingS x₁ = Trace sc x

      


    TraceNBS sc _ =
       (𝟚 , λ x X → Cubical.Data.Bool.if x then X else Unit)
        , λ {Ty'} {b} → 𝟚-elim {A = λ x → Cubical.Data.Bool.if x then Ty' else Unit → Maybe Ty'}
           (λ _ → nothing) just b

    -- TraceNBS sc (AST.NBS-require! x) = 𝟚 , {!!}
    -- TraceNBS sc (AST.NBS-deposit! x x₁) = 𝟚 , {!!}
    -- TraceNBS sc (AST.NBS-withdraw! x x₁) = 𝟚 , {!!}

    TraceE' sc e  with proj₁ (snd (IsPureE e))
    ... | yes p = Unit , λ _ → just (evalPureExpr e p)
    ... | no ¬p = TraceE sc e ¬p

    TraceE sc (AST.var (AST.dsot name {inr (x₁ , ())})) 
    TraceE sc (AST.body (AST.bodyR []L expr₁)) x = TraceE sc expr₁ (x ∘ (_ ,_))
    TraceE sc (AST.body (AST.bodyR (h ∷L stmnts₁) expr₁)) x = TraceB sc h (bodyR stmnts₁ expr₁) x
    TraceE sc (AST.lit x₁) x = empty-elim (x tt)
    TraceE sc {Τ} (AST.input x₁) x = Maybe (GTypeAgdaRep Τ) , idfun _

   -- TODO : more finegrained definition, optimising size of result, maybe spereate one, with proven equivalence? since
   -- optimised one may be not that easy to reason with. Maybe parametrise definiton by Interval?
    TraceE sc {Τ} (AST.if e then e₁ else e₂) x with proj₁ (snd (IsPureE e))
    ... | yes p =
           let v = evalPureExpr e p
           in Cubical.Data.Bool.if v
                then TraceE' sc e₁
                else TraceE' sc e₂
--       let e' = TraceE sc e ¬p
--       in Σ (fst e')
--              (recMaybe {A = 𝟚} Unit
--                   (λ b → Cubical.Data.Bool.if b
--                            then {!TraceE sc w1!}
--                            else {!!}
--                            )
-- --                (λ b → (Bool→Type b × {!!})  ⊎ (Bool→Type (not b) × {!!}) )
--                   ∘ (snd e'))
--             , {!!}

                     -- (𝟚-elim  {A = λ z →
                     --                   fst
                     --                   (TraceE' sc (if lit z then e₁ else e₂)) →
                     --                   Maybe (GTypeAgdaRep Τ)}
                     --      {!!}
                     --      {!!} )


    ... | no ¬p =
      let e' = TraceE sc e ¬p
          -- e₁' = TraceE' sc {Τ} e₁
          -- e₂' = TraceE' sc {Τ} e₂
          h = λ (b : Maybe 𝟚) →
               recMaybe 
                 (Unit , λ x₁ → nothing)
                 (λ b → TraceE' sc {Τ} (AST.if lit b then e₁ else e₂)) b
                  
      in Σ (fst e')
             (( fst ∘ h ) ∘ snd e')
            , (λ {a} b →
                 
                  maybe-elim {B = λ b' → fst (h b') → Maybe (GTypeAgdaRep Τ)}
                     (const nothing)
                     (λ b → snd (TraceE' sc (if lit b then e₁ else e₂)))
                     (snd e' (fst a)) (snd a)) ∘ snd 

    -- ... | no ¬p | w1 | w2 =
    --   let e' = TraceE sc e ¬p
    --       h = λ (b : 𝟚) → TraceE sc {Τ} (AST.if lit b then e₁ else e₂) λ x₁ → {!!}
    --   in Σ (fst e')
    --          ({! fst (TraceE sc {Τ} (AST.if e then e₁ else e₂) ?!} ∘ snd e')
    --         , {!!}


    TraceB sc {Τ} (AST.bindingS (AST.BS-let ce x₁)) bo@(AST.bodyR stmnts₁ expr₁) xx with proj₁ (snd (IsPureE x₁))
    ... | yes p =  let v = evalPureExpr x₁ p
                       -- stmnts₁' = substOneStmnts (inl v) (mkStatements* stmnts₁)
                       -- expr₁' = subst (λ x → Expr x Τ) (substOneStmnts-coh-list (inl v) stmnts₁)
                       bo' = substOneExpr {Γ = con  [ ce ] sc} {Τ = Τ} (inl v) (body bo) 
                   in TraceE' sc {Τ} (substOneExpr (inl v) (body bo))

    ... | no ¬p =
      let q = (TraceE _ x₁ ¬p)
           
      in Σ (fst q)
             (((recMaybe Unit
                λ v → let bo' = substOneExpr {Γ = con  [ ce ] sc} {Τ = Τ} (inl v) (body bo)
                      in fst (TraceE' sc {Τ} (substOneExpr (inl v) (body bo)))
                ) ∘ snd q))
           ,
           λ x →
             maybe-elim
                {B = λ b' → (recMaybe Unit
                λ v → let bo' = substOneExpr {Γ = con  [ ce ] sc} {Τ = Τ} (inl v) (body bo)
                      in fst (TraceE' sc {Τ} (substOneExpr (inl v) (body bo)))
                ) b' → Maybe (GTypeAgdaRep Τ) }
                  (λ x₂ → nothing)
                  (λ v →
                      let bo' = substOneExpr {Γ = con  [ ce ] sc} {Τ = Τ} (inl v) (body bo)
                      in snd (TraceE' sc {Τ} (substOneExpr (inl v) (body bo)))
                  )
                  ((snd q (fst x))) (snd x)
                   -- (snd q (fst x)) 
    
    TraceB sc (AST.bindingS (AST.BS-publish! p (AST.psof name {()}))) (AST.bodyR stmnts₁ expr₁) x₁
    
    TraceB sc (AST.nonBindingS (AST.stmntNBS x)) bo@(AST.bodyR stmnts₁ expr₁) xx =
       let z = TraceNBS sc x
           q = TraceE' sc (body bo)
        in Σ (fst (fst z)) (λ x₁ → snd (fst z) x₁ (fst q)) ,
              λ x₁ → bindMaybe (( snd z {_} {fst x₁} ((snd x₁)))) (snd q) 
    TraceB sc (AST.nonBindingS (AST.exprNBS x)) bo@(AST.bodyR stmnts₁ expr₁) xx =
        let nbe' = TraceE' sc x
            bo' = TraceE' sc (body bo)
        in Σ (fst nbe') (caseMaybe Unit (fst bo') ∘ snd nbe')
             , λ x₁ → maybe-elim {B = λ b'' → (caseMaybe Unit (fst bo') b'') → Maybe (GTypeAgdaRep _)}
                         (λ x₂ → nothing)
                         (λ _ → snd bo')
                         (snd nbe' (fst x₁)) (snd x₁)






           -- Trace sc []L = Unit
    -- Trace sc ss@(bindingS (BS-let ce x₁) ∷L x) with proj₁ (snd (IsPureE x₁))
    -- ... | yes p =
    --       let v = evalPureExpr x₁ p
    --           e' = substOneStmnts (inl v) (mkStatements* x) 
    --       in Trace sc e'

    -- ... | no ¬p = Σ (fst (TraceE _ x₁ ¬p))
    --             ((recMaybe Unit λ v → Trace _ (substOneStmnts (inl v) (mkStatements* x))) ∘ snd (TraceE _ x₁ ¬p))
       
    -- Trace sc ss@(AST.bindingS (AST.BS-publish! p (AST.psof name {()})) ∷L x)
    
    -- Trace sc ss@(AST.nonBindingS (AST.stmntNBS x₁) ∷L x) =
    --   Σ (fst (TraceNBS sc x₁)) (λ x₂ → (snd (TraceNBS sc x₁)) x₂ (Trace sc x))
    -- Trace sc ss@(AST.nonBindingS (AST.exprNBS {Τ} x₁) ∷L x) with proj₁ (snd (IsPureE x₁))
    -- ... | yes p = Trace sc x


    -- ... | no ¬p = Σ (fst (TraceE _ x₁ ¬p))
    --                 (caseMaybe {A = GTypeAgdaRep Τ} Unit (Trace _ x) ∘ (snd (TraceE _ x₁ ¬p)))






      -- dec-rec _ {{proj₁ (snd (IsPureStmnts ss))}}
      --   (λ x₁ → {!!})
      --   (λ x₁ → {!!})


-- data G (A : Type₀) : Type₁ where
--   input : ∀ {A'} → String → {{IsGlowTy A'}} → A ≡ Maybe A'  → G A
--   withdraw : 𝟚 ≡ A → G A
--   deposit : 𝟚 ≡ A → G A
--   _>>=_ : ∀ {A'} → G A' → (A' → G A) → G A
--   -- end : A ≡ Unit → G A
  
-- input' : String → (Τ : GType) → G (Maybe (GTypeAgdaRep Τ)) 
-- input' x Τ = input x {{GTypeAgdaRep' Τ}} refl

-- -- _>>=_ : ∀ {A B : Type₀} → G A → (A → G B) → G B
-- -- _>>=_ = {!!}

-- _>>_ : ∀ {A B : Type₀} → G A → G B → G B
-- x >> x₁ = x >>= const x₁


-- -- doTest : G {!!}
-- -- doTest = do
-- --    z ← input' "xxx" Bool
-- --    g z
-- negTest' : ∀ {A} → G A → Type₀


-- exec : ∀ {A} → (x : G A) → (negTest' x) → A

-- negTest' (input {A'} x x₁) = Maybe A'
-- negTest' (withdraw x) = 𝟚
-- negTest' (deposit x) = 𝟚
-- negTest' (x >>= x₁) = Σ (negTest' x) λ x₂ → negTest' (x₁ (exec x x₂))
-- -- negTest' (end x) = Unit

-- exec {A} (input x x₂) = transport⁻ x₂
-- exec {A} (withdraw x) = transport x
-- exec {A} (deposit x) = transport x
-- exec {A} (x >>= x₂) x₁ =
--   let w = exec x (fst x₁)
--   in exec _ (snd x₁)
-- -- exec {A} (end x) = transport⁻ x 

-- -- negTest : G Empty → Type₀
-- -- negTest (input x x₁) = {!!}
-- -- negTest (withdraw x) = {!!}
-- -- negTest (deposit x) = {!!}
-- -- negTest (x >>= x₁) = {!!}
-- -- negTest (end x) = {!!}


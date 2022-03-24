
{-# OPTIONS --cubical  #-}
module Glow.Simple.ExecTrace where

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

open import Glow.Simple.ParamsSubst

-- open import Glow.Simple.Monad


open import Cubical.HITs.Interval

infixr 4 _↦_






-- data EState : Type₀ where
--   fail ok : EState

-- data ΣM (A : EState → Type₀) (B : A ok → EState → Type₀) : EState → Type₀ where
--   _□ : (A fail) → ΣM A B fail  
--   _↦_ : (a : A ok) → ∀ {st} → B a st → ΣM A B st

-- fst-ΣM : {A : EState → Type₀} {B : A ok → EState → Type₀} → ΣM A B ok → A ok
-- fst-ΣM (a₁ ↦ x) = a₁


-- snd-ΣM : {A : EState → Type₀} {B : A ok → EState → Type₀} → (ab : ΣM A B ok) → B (fst-ΣM ab) ok
-- snd-ΣM (a₁ ↦ x) = x

-- data ×M (A : EState → Type₀) (B : EState → Type₀) : EState → Type₀ where
--   _□ : (A fail) → ×M A B fail  
--   _↦_ : (a : A ok) → ∀ {st} → B st → ×M A B st

-- fst-×M : {A : EState → Type₀} {B : EState → Type₀} → ×M A B ok → A ok
-- fst-×M (a₁ ↦ x) = a₁


-- snd-×M : {A : EState → Type₀} {B : EState → Type₀} → ×M A B ok → B ok
-- snd-×M (a₁ ↦ x) = x


-- data Withdraw {Identifier : Type₀} : Identifier → ℕ → EState → Type₀ where
--   w!_⤆_ : ∀ s → ∀ k → Withdraw s k ok
--   ¬w!_⤆_ : ∀ s → ∀ k → Withdraw s k fail

-- data Deposit {Identifier : Type₀} : Identifier → ℕ → EState → Type₀ where
--   d!_⤇_ : ∀ s → ∀ k → Deposit s k ok
--   ¬d!_⤇_ : ∀ s → ∀ k → Deposit s k fail

-- data Require : 𝟚 → EState → Type₀ where
--   r!_ : ∀ b → {Bool→Type b}  → Require b ok
--   ¬r!_ : ∀ b → {Bool→Type (not b)} → Require b fail
--   -- failed-deposit_⟶_ : ∀ s → ∀ k → Deposit s k fail

-- Require-ok→proof : ∀ {b} → Require b ok → b ≡ true
-- Require-ok→proof ((r! true) {y}) = refl

-- data Input {Identifier : Type₀} : Identifier → GType → EState → Type₀ where
--   _inp_ : ∀ s → ∀ {gt} → GTypeAgdaRep gt → Input s gt ok
--   ¬inp_ : ∀ s → ∀ {gt}  → Input s gt fail

-- data ReceivePublished {Identifier : Type₀} : Identifier → GType → EState → Type₀ where
--   _rec_ : ∀ s → ∀ {gt} → GTypeAgdaRep gt → ReceivePublished s gt ok
--   ¬rec_ : ∀ s → ∀ {gt}  → ReceivePublished s gt fail


-- data Publish {Identifier : Type₀} : Identifier → Identifier → EState → Type₀ where
--   p!_⤇_ : ∀ pa → ∀ vn → Publish pa vn ok
--   ¬p!_⤇_ : ∀ pa → ∀ vn → Publish pa vn fail


-- ok-input-elim : {Identifier : Type₀} → ∀ x Τ → Input {Identifier} x Τ ok → GTypeAgdaRep Τ 
-- ok-input-elim x Τ (.x inp x₁) = x₁

-- ok-rec-elim : {Identifier : Type₀} → ∀ x Τ → ReceivePublished {Identifier} x Τ ok → GTypeAgdaRep Τ 
-- ok-rec-elim x Τ (.x rec x₁) = x₁


-- data Branch (A-t A-f : Type₀) : 𝟚 → Type₀ where
--   br-T : ∀ {b} → (prf-T : b ≡ true) → {Bool→Type b} → A-t → Branch A-t A-f b
--   br-F : ∀ {b} → (prf-F : b ≡ false) → {Bool→Type (not b)} → A-f → Branch A-t A-f b

-- branch-elim : ∀ {A-t A-f B : Type₀} → (A-t → B) → (A-f → B) →  ∀ {b} → Branch A-t A-f b → B 
-- branch-elim x x₁ (br-T _ x₂) = x x₂
-- branch-elim x x₁ (br-F _ x₂) = x₁ x₂

-- data End : Type₀ where
--   ▣ : End


-- module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
--             {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
--               {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where


--   module TraceNice {ptps : List (Identifier × ParticipantModality)} {uniquePtps : _} where

--     open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps [] {_} {uniquePtps}) 


--     open SubstAll {Identifier} {builtIns = builtIns} {ptps = ptps}
--     open SubstOne {Identifier} {builtIns = builtIns} {ptps = ptps}

--     GMO : ∀ Τ → Type₁
--     GMO Τ = (Σ (EState → Type₀) λ x → x ok → GTypeAgdaRep Τ) 

--     returnGMO : ∀ {Τ} → GTypeAgdaRep Τ → GMO Τ
--     fst (returnGMO x) = const Unit
--     snd (returnGMO x) = const x

--     {-# TERMINATING #-}
--     Trace : ∀ sc → Statements (con [] sc) → EState → Type₀
--     TraceB : ∀ sc {Τ} → (s : Stmnt (con [] sc))
--                       → (bd : Body (bindingMechanics' _ s ) Τ)
--                       → IsEmpty ⟨ IsPureE (body (AST.bodyR (s ∷L (stmnts bd)) (expr bd))) ⟩ 
--                     → GMO Τ
--                     -- λ Tr →  (Tr → (Maybe (GTypeAgdaRep Τ)))

--     TraceNBS+Expr : ∀ sc → (s' : NBStmnt+Expr (con [] sc)) → IsEmpty ⟨ IsPureS (AST.nonBindingS s') ⟩ → GMO Unitᵍ

--     TraceNBS : ∀ sc → NBStmnt (con [] sc) → GMO Unitᵍ
--        -- Σ (Σ Type₀ λ Ty → Ty → Type₀ → Type₀)
--        --    λ x → ∀ {Ty'} → {y : (fst x)} →  ((λ b → b Ty') ∘ (snd x)) y
--        --                     → Maybe Ty' 

--     TraceEAlways : ∀ sc → ∀ {Τ} → (e : Expr (con [] sc) Τ)
--                                   → GMO Τ

--     TraceE : ∀ sc → ∀ {Τ} → (e : Expr (con [] sc) Τ) → IsEmpty ⟨ IsPureE e ⟩
--                                   → GMO Τ
--                                   -- Σ Type₀ λ Tr → (Tr → (Maybe (GTypeAgdaRep Τ)))

--     TraceE' : ∀ sc → ∀ {Τ Τ'} → (e : Expr (con [] sc) Τ) 
--                                   → (GTypeAgdaRep Τ → GMO Τ') → GMO Τ'

--     -- TraceE'' : ∀ sc → ∀ {Τ} → (e : Expr (con [] sc) Τ) 
--     --                               → GMO Τ ⊎ GTypeAgdaRep Τ


--     TraceE* : ∀ sc → ∀ {Τ Τ'} → (e : Expr (con [] sc) Τ) → IsEmpty ⟨ IsPureE e ⟩
--                                   → (GTypeAgdaRep Τ → GMO Τ') → GMO Τ'

--     TraceE*' : ∀ sc → ∀ {Τ Τ'} → (e : Expr (con [] sc) Τ) → IsEmpty ⟨ IsPureE e ⟩
--                                   → (GTypeAgdaRep Τ → GMO Τ') ⊎ (GTypeAgdaRep Τ → GTypeAgdaRep Τ') → GMO Τ'


--     -- TraceE'' : ∀ sc → ∀ {Τ Τ'} → (e : Expr (con [] sc) Τ) 
--     --                                → GMO Τ'


--     Trace sc []L fail = Empty
--     Trace sc []L ok = End
--     Trace sc (h ∷L x) with proj₁ (snd (IsPureS h))
--     ... | no ¬p = fst (TraceB sc h (AST.bodyR x (lit tt)) (¬p ∘ proj₁ ∘ proj₁))
--     ... | yes p with h
--     ... | AST.bindingS (AST.BS-let ce x₁) =
--                 Trace sc (substOneStmnts (inl (evalPureExpr x₁ p)) (mkStatements* x))
--     ... | AST.nonBindingS x₁ = Trace sc x
--     -- Trace sc []L = End
--     --   -- Unit
--     -- Trace sc (h ∷L x) with proj₁ (snd (IsPureS h))
--     -- ... | no ¬p = fst (TraceB sc h (AST.bodyR x (lit tt)) (¬p ∘ proj₁ ∘ proj₁))
--     -- ... | yes p with h
--     -- ... | AST.bindingS (AST.BS-let ce x₁) =
--     --           Trace sc (substOneStmnts (inl (evalPureExpr x₁ p)) (mkStatements* x))
--     -- ... | AST.nonBindingS x₁ = Trace sc x

      

--     TraceNBS+Expr sc (AST.stmntNBS x₁) x = TraceNBS sc x₁
--     TraceNBS+Expr sc (AST.exprNBS x₁) x =
--        let q = TraceE sc x₁ x
--        in fst q , const _
    
--     TraceNBS sc (AST.NBS-require! x) = TraceE' sc x λ b → Require b , λ _ → tt
--     TraceNBS sc (AST.NBS-deposit! (AST.pId name) x₁) = TraceE' sc x₁ λ k → Deposit name k , λ _ → tt
--     TraceNBS sc (AST.NBS-withdraw! (AST.pId name) x₁) = TraceE' sc x₁ λ k → Withdraw name k , λ _ → tt
--     TraceNBS sc (AST.NBS-publishVal! (AST.pId name) x) = Publish name x , λ _ → tt
    
--     TraceE* sc e ¬p q =
--         let e' = TraceE sc e ¬p
--         in ΣM (fst e') (fst ∘ q ∘ snd e') , λ y → snd (q ( snd e' (fst-ΣM y))) (snd-ΣM y)


--     TraceE' sc e q with proj₁ (snd (IsPureE e))
--     ... | yes p = q (evalPureExpr e p)
              
--     ... | no ¬p = TraceE* sc e ¬p q

--     TraceE*' sc e x (inl x₁) = TraceE* sc e x x₁
--     TraceE*' sc e x (inr x₁) =
--        let q = TraceE sc e x
--        in (fst q) , x₁ ∘ snd q


--     TraceE sc (AST.var (AST.dsot name {inr (x₁ , ())})) x
--     TraceE sc (AST._$_ _ _) x = empty-elim (x _)
--     TraceE sc (AST.body (AST.bodyR []L expr₁)) x = TraceE sc expr₁ (x ∘ (_ ,_))
--     TraceE sc (AST.body (AST.bodyR (h ∷L stmnts₁) expr₁)) x = TraceB sc h (bodyR stmnts₁ expr₁) x
--     TraceE sc (AST.lit x₁) x = empty-elim (x tt)
--     TraceE (just (AST.pId name)) {Τ} (AST.input x₁) x = Input name Τ  , ok-input-elim _ _
--     TraceE sc {Τ} (AST.receivePublished (AST.pId name)) x = ReceivePublished name Τ  , ok-rec-elim _ _
--     TraceE sc (AST.if e then e₁ else e₂) x = h (proj₁ (snd (IsPureE e₁))) (proj₁ (snd (IsPureE e₂)))
--       where
--         h' : Σ (EState → Type) (λ x₁ → x₁ ok → GTypeAgdaRep _)
--         h' =  let e₁' = TraceEAlways sc e₁
--                   e₂' = TraceEAlways sc e₂
--               in TraceE' sc e
--                     λ b → (λ es → Branch (fst e₁' es) ((fst e₂' es)) b)
--                      , branch-elim (snd e₁') (snd e₂')
        

--         h : Dec (fst (IsPureE e₁)) → Dec (fst (IsPureE e₂)) → GMO _
--         h (yes p) (yes p₁) =
--           let q = (TraceE sc e λ x₁ → x (x₁ , (p , p₁))) 
--           in fst q , λ x₁ → Cubical.Data.Bool.if (snd q x₁)
--                               then evalPureExpr e₁ p
--                               else evalPureExpr e₂ p₁
--         h (yes p) (no ¬p) = h'
--         h (no ¬p) (yes p) = h'
--         h (no ¬p) (no ¬p₁) = h'
--     TraceE sc (AST.sign x₁) x = empty-elim (x tt)

--     TraceEAlways sc e with (proj₁ (snd (IsPureE e)))
--     ... | yes p = returnGMO (evalPureExpr e p)
--     ... | no ¬p = TraceE _ e ¬p


--     postulate never : Empty 

--     promiseThatTrue : ∀ {A} → Dec A → A
--     promiseThatTrue (yes p) = p
--     promiseThatTrue (no ¬p) = empty-elim never

--     TraceB sc {Τ} (AST.bindingS (AST.BS-let ce x₁)) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureE x₁)))
--     ... | yes p =
--        let v = evalPureExpr x₁ p
--        in TraceE _ ((substOneExpr (inl v) (body bo))) λ x → never
--     ... | no ¬p with (proj₁ (snd (IsPureE (body bo))))
--     ... | yes p = TraceE*' _ x₁ ¬p (inr λ x → evalPureExpr (substOneExpr (inl x) (body bo))
--                        (promiseThatTrue ((proj₁ (snd (IsPureE ((substOneExpr (inl x) (body bo)))))))))
--     ... | no ¬p₁ = TraceE*' _ x₁ ¬p (inl λ x → TraceE _ (substOneExpr (inl x) (body bo)) λ x₂ → never)


--     TraceB sc (AST.bindingS (AST.BS-publish! p (AST.psof name {()}))) (AST.bodyR stmnts₁ expr₁) x₁

--     TraceB sc s@(AST.nonBindingS y) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureS s))) | (proj₁ (snd (IsPureE (body bo)))) 
--     ... | yes p | _ = TraceE _ (body bo) λ x → xx ((p , proj₁ x) , proj₂ x)
--     ... | no ¬p | yes p = (fst (TraceNBS+Expr _ y ¬p))
--                               , λ _ → evalPureExpr (body bo) p
--     ... | no ¬p | no ¬p₁ =
--                    let z = TraceNBS+Expr _ y ¬p
--                        q = TraceE sc (body bo) ¬p₁
--                    in ×M (fst z) (fst q) ,
--                         snd q ∘ snd-×M


--   module Helpers where

--     -- open AST.InteractionHead {Identifier} {builtIns = builtIns} {one}  


--     open SubstAll {Identifier} {builtIns = builtIns}
--     open SubstOne {Identifier} {builtIns = builtIns}
--     open ParamsSubst {Identifier} {builtIns = builtIns}

--     genTracesType : AST.Interaction Identifier builtIns one → Σ Type₀ λ x → x → EState → Type₀ 
--     genTracesType (AST.interaction head code) =
--        AST.ParametersValue Identifier builtIns one (AST.InteractionHead.parameters head) ,
--           λ paramsV → TraceNice.Trace {(AST.InteractionHead.participantsWM head)} nothing (paramSubst paramsV code)


--   open Helpers public

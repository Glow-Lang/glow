{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.ControlFlowTransHLP where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat renaming (_+_ to _ℕ+_)
open import Cubical.Data.Fin
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

open import Glow.ListDecProps

open import Cubical.Categories.Category

open import Glow.Simple.Lurk.HaskellInterface

open import Glow.Simple.Lurk.ControlFlowTrans

module ToLurkCF where

  open import Glow.Simple.ASTDef


  open AST-String one

  -- open AST

  open MonadicControlFlow.MonadicControlFlowUP {String} {builtIns = Basic-BuiltIns} ("A" ∷ "B" ∷ [])
     (toWitness {Q = UniqueByDec≡ proj₁ (map-List (_, false) ("A" ∷ "B" ∷ []))} tt)

  idA idB : PID
  idA = pId "A" {toWitnessDP ((IsDishonestParticipantId {ptps''} "A")) tt}
  idB = pId "B" {toWitnessDP ((IsDishonestParticipantId {ptps''} "B")) tt}

  noBind : ∀ {A Γ Τ} → LMonad A Γ Unitᵍ → LMonad A (Γ , "nobind" ⦂ Unitᵍ) Τ → LMonad A Γ Τ
  noBind = bind

  testLM : LMonad ℕ (con [] nothing) Unitᵍ 
  testLM = bind {x = "z"} {Τ' = Nat} (noBind (noBind (action 0 idA (withdrawA < 2 > ) refl)
                      (action 1 idB (depositA < 2 > ) refl))
                  (pure < 4 >))
                 (bind {x = "zz"} {Τ' = Nat} (expectPub 2 idA)
                     (action 3 idB (withdrawA < 3 >) refl))



  
  

  toLurkGlowcode : ∀ Γ Τ → LMonad ℕ Γ Τ → L.Expr
  toLurkGlowcode Γ Τ x = {!testLM!}



      -- data Action (Γ : Context) : Type₀ where
      --   withdrawA : ℕ → PID → Expr Γ Nat → Action Γ
      --   depositA : ℕ → PID → Expr Γ Nat → Action Γ 
      --   -- branching : PCExpr pc Bool → List (Action Γ) → List (Action Γ) → Action Γ

      -- record Actions (Γ : Context) (Τ : GType) : Type₀


      -- data AEnd (Γ : Context) (Τ : GType) : Type₀ where
      --   pureA : ℕ → AEnd Γ Τ
      --   publishA : ℕ → PID → AEnd Γ Τ
      --   letE : {!!} → {!!} →  AEnd Γ Τ
      --   ifA : Expr Γ Bool → Actions Γ Τ → Actions Γ Τ → AEnd Γ Τ
      --   bindA : ∀ Τ' → Actions Γ Τ' → {!!} →  AEnd Γ Τ
 

      -- record Actions Γ Τ where
      --   coinductive
      --   field
      --     actions : List (Action Γ)
      --     aend : AEnd Γ Τ








      --   withdrawA : ℕ → PCExpr pc Nat → Action pc
      --   depositA : ℕ → PCExpr pc Nat → Action pc 
      --   branching : PCExpr pc Bool → List (Action pc) → List (Action pc) → Action pc



      -- LurkIR : PublicContext → Type₀
      -- LurkIR pc = List (Action pc) × Maybe {!LurkIR!}

      -- data Action (pc : PublicContext) : Type₀ where
      --   withdrawA : PCExpr pc Nat → Action pc
      --   depositA : PCExpr pc Nat → Action pc 
      --   publishA : Identifier → GType → Action pc





      



      -- -- AMM : (A : Type₀) → PublicContext →  Type₀ 
      -- -- MMM : PublicContext →  Type₀ 
      -- -- data MM (A : Type₀) (pc : PublicContext) : Type₀ where
      -- --    guardMM : A → DishonestParticipantId → PCExpr pc Bool → Action pc → MM A pc
      -- --    branchMM : PCExpr pc Bool → AMM A pc → AMM A pc → MM A pc






      -- -- -- publicBindingPart : ∀ pc → MM pc → PublicContext
      -- -- -- publicBindingPart pc (guardMM x (withdrawA x₁)) = PC[]
      -- -- -- publicBindingPart pc (guardMM x (depositA x₁)) = PC[]
      -- -- -- publicBindingPart pc (guardMM x (publishA x₁ x₂)) = inl (x₂) PC∷ PC[]
      -- -- -- publicBindingPart pc (branchMM x x₁ x₂) = 
      -- -- --    inr ({!!} , {!!}) PC∷ PC[]

      -- -- {-# TERMINATING #-}
      -- -- publicBinding : ∀ {A} → ∀ pc → MM A pc → PublicContext

      -- -- AMM A = Linked' (publicBinding {A})


      -- -- MMM = Linked' (publicBinding {Unit})

      -- -- NMM = Linked' (publicBinding {ℕ})

      -- -- publicBinding pc (guardMM _ _ x (withdrawA x₁)) = PC[]
      -- -- publicBinding pc (guardMM _ _ x (depositA x₁)) = PC[]
      -- -- publicBinding pc (guardMM _ _ x (publishA x₁ x₂)) = inl (x₂) PC∷ PC[] 
      -- -- publicBinding pc (branchMM x x₁ x₂) =
      -- --    inr (foldLinked' x₁ , foldLinked' x₂) PC∷ PC[]

      -- -- CC : ∀ {_ : Type₀} → Category ℓ-zero ℓ-zero
      -- -- CC {A} = FreeCategory' {C = PublicContext} {A = MM A} publicBinding {!!} {!!}




      -- -- -- testMMM : MMM PC[]
      -- -- -- testMMM = guardMM {!!} (publishA {!!} Nat) ∷L ({!!}  ∷L []L)

      -- -- mkGuard : (ss : Statements (con [] nothing)) → PCExpr (StatementsPC ss) Bool
      -- -- mkGuard = {!!}

      -- -- evalViaPublic : ∀ {Τ} → (ss : Statements (con [] nothing)) → Expr (foldLinked' ss) Τ → PCExpr (StatementsPC ss) Τ 
      -- -- evalViaPublic = {!!}

      -- -- {-# TERMINATING #-}
      -- -- sliceMM : (ss : Statements (con [] nothing))
      -- --           → Statements (foldLinked' ss)
      -- --           → MMM (StatementsPC ss)
      -- -- sliceMM ss []L = []L
      -- -- sliceMM ss (h ∷L []L) = w h
      -- --    where

      -- --     w'' : NBStmnt (foldLinked' ss) → Maybe (Action (StatementsPC ss))
      -- --     w'' (AST.NBS-require! x) = nothing
      -- --     w'' (AST.NBS-deposit! x x₁) = just (depositA (evalViaPublic ss x₁))
      -- --     w'' (AST.NBS-withdraw! x x₁) = just (withdrawA (evalViaPublic ss x₁))
      -- --     w'' (AST.NBS-publishVal! x x₁) = nothing -- TODO: properly handle as imposible! 


      -- --     w : Stmnt (foldLinked' ss) → MMM (StatementsPC ss)
      -- --     w (AST.bindingS (AST.BS-let (AST.ice nothing name type) x)) = w (AST.nonBindingS (AST.exprNBS {!!}))
      -- --     w (AST.bindingS (AST.BS-let (AST.ice (just x₁) name type) x)) = []L -- TODO: properly handle as imposible! 
      -- --     w (AST.bindingS (AST.BS-publish! p x)) = []L -- TODO: properly handle as imposible! 
      -- --     w (AST.nonBindingS (AST.stmntNBS x)) with (w'' x)
      -- --     ... | nothing = []L
      -- --     ... | just x₁ = guardMM _ {!!} (mkGuard ss) x₁ ∷L []L
      -- --     w (AST.nonBindingS (AST.exprNBS x)) = {!x!}
          
      -- -- sliceMM ss (h ∷L (h₁ ∷L x)) =
      -- --    let q = sliceMM ss (h ∷L []L)
      -- --        ss' = Category._⋆_ STMNTS (ss , refl) ((h ∷L []L) , refl)
      -- --        q' = sliceMM (fst ss') (subst (Linked' bindingMechanics') (snd ss') (h₁ ∷L x))
      -- --        z = (Category._⋆_ CC (q , {!!}) (q' , refl ))
      -- --    in fst z

      -- --   where
      -- --     open FreeCategory (publicBinding {A = Unit})
      -- -- -- popMM : Statements (con [] nothing) → Maybe {!!}
      -- -- -- popMM = {!!}

      -- -- MMM' = MMM PC[]

      -- -- mkMM : Statements (con [] nothing) → MMM'
      -- -- mkMM = sliceMM []L

      -- -- countStates : ∀ {A} → ∀ pc → AMM A pc → ℕ
      -- -- countStates pc []L = zero
      -- -- countStates pc (guardMM _ _ x₁ x₂ ∷L x) = suc (countStates _ x)
      -- -- countStates pc (branchMM x₁ h h₁ ∷L x) =   countStates _ h ℕ+ countStates _ h₁ ℕ+ countStates _ x


      -- -- MMM→NMM-h : ℕ → ∀ pc → MMM pc → NMM pc
      -- -- MMM→NMM-h _ _ []L = []L
      -- -- MMM→NMM-h k pc (guardMM _ x₁ x₂ y@(withdrawA x₃) ∷L x) = (guardMM k x₁ x₂ y ∷L MMM→NMM-h (suc k) _ x)
      -- -- MMM→NMM-h k pc (guardMM _ x₁ x₂ y@(depositA x₃) ∷L x) = (guardMM k x₁ x₂ y ∷L MMM→NMM-h (suc k) _ x)
      -- -- MMM→NMM-h k pc (guardMM _ x₁ x₂ y@(publishA x₃ x₄) ∷L x) = (guardMM k x₁ x₂ y ∷L MMM→NMM-h (suc k) _ x)
      -- -- MMM→NMM-h k pc (branchMM x₁ x₂ x₃ ∷L x) =
      -- --    let b1 = MMM→NMM-h k pc x₂
      -- --        b2 = MMM→NMM-h (countStates _ x₂ ℕ+ k) pc x₃
      -- --     in (branchMM x₁ b1 b2 ∷L {!MMM→NMM-h (countStates _ x₂ ℕ+ countStates _ x₃) _ x!})
            
      -- -- MMM→NMM : ∀ pc → MMM pc → NMM pc
      -- -- MMM→NMM = MMM→NMM-h 0



      -- -- record StateInfo  : Type₀ where
      -- --   field
      -- --     stateContext : PublicContext
      -- --     action : Action stateContext
      -- --     caller : DishonestParticipantId
          


      -- -- record ReachFoldState : Type₀ where
      -- --   field
      -- --     openStates : List ℕ
      -- --     statesInfo : List (ℕ × (StateInfo)) 
      -- --     computed : ℕ → List (ℕ)

      -- -- open ReachFoldState

      -- -- initReachFoldState : ReachFoldState
      -- -- openStates initReachFoldState = []
      -- -- statesInfo initReachFoldState = []
      -- -- computed initReachFoldState = const []

      -- -- openState : ℕ → StateInfo → ReachFoldState → ReachFoldState
      -- -- openStates (openState x y x₁) = x ∷ (openStates x₁) 
      -- -- statesInfo (openState x y x₁) = (x , y) ∷ statesInfo x₁
      -- -- computed (openState x y x₁) = computed x₁


      
      -- -- _isIn_ : ℕ → List ℕ → 𝟚
      -- -- x isIn [] = false
      -- -- x isIn (x₁ ∷ x₂) = x isIn x₂ or Dec→Bool (discreteℕ x x₁)

      -- -- _Li∪_ : List ℕ → List ℕ → List ℕ
      -- -- [] Li∪ x₁ = x₁
      -- -- (x ∷ xs) Li∪ y =
      -- --    Cubical.Data.Bool.if x isIn y
      -- --      then xs Li∪ y
      -- --      else x ∷ xs Li∪ y

      -- -- _Li∪'_ : ∀ {A : Type₀} → List (ℕ × A) → List (ℕ × A) → List (ℕ × A)
      -- -- [] Li∪' x₁ = x₁
      -- -- (x ∷ xs) Li∪' y =
      -- --   Cubical.Data.Bool.if proj₁ x isIn map-List proj₁ y
      -- --      then xs Li∪' y
      -- --      else x ∷ xs Li∪' y


      -- -- registerState : ℕ → ReachFoldState → ReachFoldState
      -- -- openStates (registerState x x₁) = (openStates x₁)
      -- -- statesInfo (registerState x x₁) = statesInfo x₁ 
      -- -- computed (registerState x x₁) k =
      -- --   Cubical.Data.Bool.if k isIn (openStates x₁)
      -- --    then x ∷ computed x₁ k
      -- --    else computed x₁ k


      -- -- convergeRFS : ReachFoldState → ReachFoldState → ReachFoldState
      -- -- openStates (convergeRFS rfsA rfsB) = (openStates rfsA) Li∪ (openStates rfsB)
      -- -- statesInfo (convergeRFS rfsA rfsB) = statesInfo rfsA Li∪' statesInfo rfsB
      -- -- computed (convergeRFS rfsA rfsB) k =
      -- --     computed rfsA k Li∪ computed rfsB k

      -- -- closeAll : ReachFoldState → ReachFoldState
      -- -- openStates (closeAll x) = []
      -- -- statesInfo (closeAll x) = statesInfo x
      -- -- computed (closeAll x) = computed x

      -- -- reachability' : ReachFoldState → ∀ pc → NMM pc → ReachFoldState

      -- -- reachabilityStep : ReachFoldState → ∀ pc → MM ℕ pc → ReachFoldState
      -- -- reachabilityStep rfs pc (guardMM x x₁ x₂ x₃) =
      -- --    openState x si (closeAll rfs)
      -- --    where
      -- --      si : StateInfo
      -- --      StateInfo.stateContext si = pc
      -- --      StateInfo.action si = x₃
      -- --      StateInfo.caller si = x₁
      -- -- reachabilityStep rfs pc (branchMM x x₁ x₂) =
      -- --    convergeRFS (reachability' rfs pc x₁) (reachability' rfs pc x₂) 


      -- -- reachability' rfs pc []L = rfs
      -- -- reachability' rfs pc (h ∷L x) = reachability' (reachabilityStep rfs _ h) _ x


      -- -- reachability : ∀ pc → NMM pc → ReachFoldState
      -- -- reachability = reachability' initReachFoldState




      -- -- -- record TranslationReady : Type₀ where
      -- -- --   field
      -- -- --     numberOfStates : ℕ
      -- -- --   StateId = Fin (numberOfStates)

      -- -- --   field
      -- -- --     stateInfo : StateId → StateInfo
        
          
      -- -- --   field
      -- -- --     reachableStates : (sId : StateId)  -- → (Γ : Rec (stateContext sId))
      -- -- --           → List (StateId × PCExpr (stateContext (stateInfo sId)) Bool)


      -- -- -- open StateInfo
      

      -- -- -- record TranslationReady : Type₀ where
      -- -- --   field
      -- -- --     numberOfStates : ℕ
      -- -- --   StateId = Fin (numberOfStates)

      -- -- --   field
      -- -- --     stateInfo : StateId → StateInfo
        
          
      -- -- --   field
      -- -- --     reachableStates : (sId : StateId)  -- → (Γ : Rec (stateContext sId))
      -- -- --           → List (StateId × PCExpr (stateContext (stateInfo sId)) Bool)


      -- -- -- open TranslationReady

      -- -- -- countStates : ∀ pc → MMM pc → ℕ
      -- -- -- countStates pc []L = zero
      -- -- -- countStates pc (guardMM _ x₁ x₂ ∷L x) = suc (countStates _ x)
      -- -- -- countStates pc (branchMM x₁ h h₁ ∷L x) =   countStates _ h ℕ+ countStates _ h₁ ℕ+ countStates _ x

      -- -- -- mkStateInfo : ∀ pc → (x : MMM pc) → Fin (countStates pc x) → StateInfo
      -- -- -- mkStateInfo pc []L = empty-rec ∘ ¬Fin0
      -- -- -- mkStateInfo pc (guardMM z x₂ x₃ ∷L x) =
      -- -- --    sum-rec (λ x₁ → record { stateContext = pc ; action = x₃ ; caller = z })
      -- -- --    (λ x₁ → mkStateInfo _ x (fst x₁)) ∘ fsplit
      -- -- -- mkStateInfo pc (branchMM x₂ x₃ x₄ ∷L x) x₁ = {!!}

      -- -- -- digForStates : {!!}
      -- -- -- digForStates = {!!}

      -- -- -- mkReachableStates : ∀ pc → (mmm : MMM pc) → (sId : Fin (countStates pc mmm))
      -- -- --      → List (Fin (countStates pc mmm) × PCExpr (stateContext (mkStateInfo pc mmm sId)) Bool)
      -- -- -- mkReachableStates pc []L = empty-rec ∘ ¬Fin0
      -- -- -- mkReachableStates pc (guardMM z x x₁ ∷L mmm) sId = {!!}
      -- -- -- mkReachableStates pc (branchMM x x₁ x₂ ∷L mmm) sId = {!!}

      -- -- -- MMM'→TR : MMM' → TranslationReady
      -- -- -- numberOfStates (MMM'→TR x) = countStates _ x
      -- -- -- stateInfo (MMM'→TR x) = mkStateInfo _ x
      -- -- -- reachableStates (MMM'→TR x) = {!!}

      -- -- -- -- --   OpenStates : Type₀
      -- -- -- -- --   OpenStates = List (StateId)

      -- -- -- -- -- open TranslationReady


      -- -- -- -- -- emptyTranslationReady : TranslationReady
      -- -- -- -- -- numberOfStates emptyTranslationReady = zero
      -- -- -- -- -- stateInfo emptyTranslationReady = empty-rec ∘ ¬Fin0
      -- -- -- -- -- reachableStates emptyTranslationReady _ = []
      
      -- -- -- -- -- TRS = Σ TranslationReady OpenStates

      -- -- -- -- -- compTRS : List TRS → TRS → TRS
      -- -- -- -- -- compTRS hs t = {!!}

      -- -- -- -- -- postulate never : Empty 


      -- -- -- -- -- {-# TERMINATING #-}
      -- -- -- -- -- foldTR : Statements (con [] nothing) → TranslationReady

      -- -- -- -- -- foldTRB : ∀ {Τ} → (s : Stmnt (con [] nothing))
      -- -- -- -- --           → (bd : Body (bindingMechanics' _ s ) Τ)
      -- -- -- -- --           → IsEmpty ⟨ IsPureE (body (AST.bodyR (s ∷L (stmnts bd)) (expr bd))) ⟩ 
      -- -- -- -- --           → TRS

      -- -- -- -- -- foldTRE : ∀ {Τ} → (e : Expr (con [] nothing) Τ)
      -- -- -- -- --           → IsEmpty ⟨ IsPureE e ⟩ 
      -- -- -- -- --           → TRS


      -- -- -- -- -- foldTR []L = emptyTranslationReady
      -- -- -- -- -- foldTR (h ∷L x) with proj₁ (snd (IsPureS h))
      -- -- -- -- -- ... | no ¬p = fst (foldTRB h (AST.bodyR x (lit tt)) (¬p ∘ proj₁ ∘ proj₁))
      -- -- -- -- -- ... | yes p with h
      -- -- -- -- -- ... | AST.bindingS (AST.BS-let ce x₁) =
      -- -- -- -- --             let y = (substOneStmnts (inl (evalPureExpr x₁ p)) (mkStatements* x))
      -- -- -- -- --             in foldTR y
      -- -- -- -- -- ... | AST.nonBindingS x₁ = foldTR x


      -- -- -- -- -- foldTRB {Τ} (AST.bindingS (AST.BS-let ce x₁)) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureE x₁)))
      -- -- -- -- -- ... | yes p =
      -- -- -- -- --    let v = evalPureExpr x₁ p
      -- -- -- -- --    in foldTRE ((substOneExpr (inl v) (body bo))) λ _ → never
      -- -- -- -- -- ... | no ¬p with (proj₁ (snd (IsPureE (body bo))))
      -- -- -- -- -- ... | yes p = {!!}
      -- -- -- -- -- ... | no ¬p₁ = {!!}

      -- -- -- -- -- foldTRB (AST.bindingS (AST.BS-publish! p (AST.psof name {()}))) (AST.bodyR stmnts₁ expr₁) x₁

      -- -- -- -- -- foldTRB s@(AST.nonBindingS y) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureS s))) | (proj₁ (snd (IsPureE (body bo)))) 
      -- -- -- -- -- ... | yes p | _ = {!!}
      -- -- -- -- -- ... | no ¬p | yes p = {!!}
      -- -- -- -- -- ... | no ¬p | no ¬p₁ = {!!}

  
      -- -- -- -- -- foldTRE e x = {!!}



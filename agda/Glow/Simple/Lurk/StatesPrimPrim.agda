{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.StatesPrimPrim where

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

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
            {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where


  module TraceNice {ptpsIds : List (Identifier)} where

    ptps : List (Identifier × ParticipantModality)
    ptps = map-List (_, dishonest) ptpsIds
    
    module _ {uniquePtps : _} where
    
      open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps [] {_} {uniquePtps}) 



      open SubstAll {Identifier} {builtIns = builtIns} {ptps = ptps}
      open SubstOne {Identifier} {builtIns = builtIns} {ptps = ptps}

   -- Context :   (A : Int) :: (B : Int) :: (C : Bool) :: []
   --             (A : 3)   :: (B : 2) :: (C : True ) :: []



   -- PublicContext : (A : Int ) :: { ( (B : Int) :: (C : Int)  ) , (D : Int)  } :: []
   --                 (A : 3) :: ( B : 2) :: (C : 4) :: []
   --                 (A : 2) :: (D : 4) :: []


      data PublicContext : Type₀ where
        PC[] : PublicContext
        _PC∷_ : (GType) ⊎ (PublicContext × PublicContext) → PublicContext → PublicContext 
        -- ⟨_∶_⟩PC∷_ : Identifier → GType → PublicContext → PublicContext
        -- [_∨_]bPC∷_ : PublicContext → PublicContext → PublicContext → PublicContext

      {-# TERMINATING #-}
      PCRec : PublicContext → Type₀
      PCRec PC[] = Unit
      PCRec (x PC∷ x₁) = (sum-elim GTypeAgdaRep (λ b → PCRec (proj₁ b) ⊎ PCRec (proj₂ b)) x) × PCRec x₁ 

      _PC++_ : PublicContext → PublicContext → PublicContext
      _PC++_ PC[] x₁ = x₁
      _PC++_ (x PC∷ x₂) x₁ = x PC∷ (x₂ PC++  x₁)
      
      StmntPC : ∀ {Γ} → Stmnt Γ → PublicContext

      StatementsPC : ∀ {Γ} → Statements Γ → PublicContext
      ExprPC : ∀ {Γ Τ} → Expr Γ Τ → PublicContext

      StatementsPC []L = PC[]
      StatementsPC (h ∷L x) = StmntPC h  PC++ StatementsPC x 

      ExprPC (AST.var x) = PC[]
      ExprPC (AST.body (AST.bodyR stmnts₁ expr₁)) = StatementsPC stmnts₁ PC++ ExprPC expr₁
      ExprPC (AST.lit x) = PC[]
      ExprPC (x AST.$' x₁) = PC[]
      ExprPC (AST.input x) = PC[]
      ExprPC (AST.sign x) = PC[]
      ExprPC {Τ = Τ} (AST.receivePublished x) = inl (Τ) PC∷ PC[]
      ExprPC (AST.if x then x₁ else x₂) = inr (ExprPC x₁ , ExprPC x₂) PC∷ PC[]

      StmntPC {Γ} (AST.bindingS x) = w x
         where
          w : BStmnt Γ → PublicContext
          w (AST.BS-let ce x) = ExprPC x
          w (AST.BS-publish! p x) = PC[] 
      StmntPC {Γ} (AST.nonBindingS (AST.stmntNBS x)) = PC[]
        -- where
        --   w : NBStmnt Γ → PublicContext
        --   w (AST.NBS-require! x) = PC[]
        --   w (AST.NBS-deposit! x x₁) = PC[]
        --   w (AST.NBS-withdraw! x x₁) = PC[]
        --   w (AST.NBS-publishVal! x x₁) = PC[] -- <- imposible!!
      StmntPC {Γ} (AST.nonBindingS (AST.exprNBS x)) = ExprPC x

      -- evalWithPC : ∀ {Γ Τ} → (e : Expr Γ Τ) → (r : Rec Γ) → PCRec (ExprPC e) → GTypeAgdaRep Τ
      -- evalWithPC = {!!}

      -- evalWithPC : ∀ {Τ} → (e : Expr (con [] nothing) Τ) → PCRec (ExprPC e) → GTypeAgdaRep Τ
      -- evalWithPC e x = {!!}


      record PCExpr (pc : PublicContext) (Τ : GType) : Type₀ where
        field
           e : Expr (con [] nothing) Τ
           pc≡ : ExprPC e ≡ pc

      data Action (pc : PublicContext) : Type₀ where
        withdrawA : PCExpr pc Nat → Action pc
        depositA : PCExpr pc Nat → Action pc 
        publishA : Identifier → GType → Action pc

      AMM : (A : Type₀) → PublicContext →  Type₀ 
      MMM : PublicContext →  Type₀ 
      data MM (A : Type₀) (pc : PublicContext) : Type₀ where
         guardMM : A → DishonestParticipantId → PCExpr pc Bool → Action pc → MM A pc
         branchMM : PCExpr pc Bool → AMM A pc → AMM A pc → MM A pc


      -- publicBindingPart : ∀ pc → MM pc → PublicContext
      -- publicBindingPart pc (guardMM x (withdrawA x₁)) = PC[]
      -- publicBindingPart pc (guardMM x (depositA x₁)) = PC[]
      -- publicBindingPart pc (guardMM x (publishA x₁ x₂)) = inl (x₂) PC∷ PC[]
      -- publicBindingPart pc (branchMM x x₁ x₂) = 
      --    inr ({!!} , {!!}) PC∷ PC[]

      {-# TERMINATING #-}
      publicBinding : ∀ {A} → ∀ pc → MM A pc → PublicContext

      AMM A = Linked' (publicBinding {A})


      MMM = Linked' (publicBinding {Unit})

      NMM = Linked' (publicBinding {ℕ})

      publicBinding pc (guardMM _ _ x (withdrawA x₁)) = PC[]
      publicBinding pc (guardMM _ _ x (depositA x₁)) = PC[]
      publicBinding pc (guardMM _ _ x (publishA x₁ x₂)) = inl (x₂) PC∷ PC[] 
      publicBinding pc (branchMM x x₁ x₂) =
         inr (foldLinked' x₁ , foldLinked' x₂) PC∷ PC[]

      CC : ∀ {_ : Type₀} → Category ℓ-zero ℓ-zero
      CC {A} = FreeCategory' {C = PublicContext} {A = MM A} publicBinding {!!} {!!}




      -- testMMM : MMM PC[]
      -- testMMM = guardMM {!!} (publishA {!!} Nat) ∷L ({!!}  ∷L []L)

      mkGuard : (ss : Statements (con [] nothing)) → PCExpr (StatementsPC ss) Bool
      mkGuard = {!!}

      evalViaPublic : ∀ {Τ} → (ss : Statements (con [] nothing)) → Expr (foldLinked' ss) Τ → PCExpr (StatementsPC ss) Τ 
      evalViaPublic = {!!}

      {-# TERMINATING #-}
      sliceMM : (ss : Statements (con [] nothing))
                → Statements (foldLinked' ss)
                → MMM (StatementsPC ss)
      sliceMM ss []L = []L
      sliceMM ss (h ∷L []L) = w h
         where

          w'' : NBStmnt (foldLinked' ss) → Maybe (Action (StatementsPC ss))
          w'' (AST.NBS-require! x) = nothing
          w'' (AST.NBS-deposit! x x₁) = just (depositA (evalViaPublic ss x₁))
          w'' (AST.NBS-withdraw! x x₁) = just (withdrawA (evalViaPublic ss x₁))
          w'' (AST.NBS-publishVal! x x₁) = nothing -- TODO: properly handle as imposible! 


          w : Stmnt (foldLinked' ss) → MMM (StatementsPC ss)
          w (AST.bindingS (AST.BS-let (AST.ice nothing name type) x)) = w (AST.nonBindingS (AST.exprNBS {!!}))
          w (AST.bindingS (AST.BS-let (AST.ice (just x₁) name type) x)) = []L -- TODO: properly handle as imposible! 
          w (AST.bindingS (AST.BS-publish! p x)) = []L -- TODO: properly handle as imposible! 
          w (AST.nonBindingS (AST.stmntNBS x)) with (w'' x)
          ... | nothing = []L
          ... | just x₁ = guardMM _ {!!} (mkGuard ss) x₁ ∷L []L
          w (AST.nonBindingS (AST.exprNBS x)) = {!x!}
          
      sliceMM ss (h ∷L (h₁ ∷L x)) =
         let q = sliceMM ss (h ∷L []L)
             ss' = Category._⋆_ STMNTS (ss , refl) ((h ∷L []L) , refl)
             q' = sliceMM (fst ss') (subst (Linked' bindingMechanics') (snd ss') (h₁ ∷L x))
             z = (Category._⋆_ CC (q , {!!}) (q' , refl ))
         in fst z

        where
          open FreeCategory (publicBinding {A = Unit})
      -- popMM : Statements (con [] nothing) → Maybe {!!}
      -- popMM = {!!}

      MMM' = MMM PC[]

      mkMM : Statements (con [] nothing) → MMM'
      mkMM = sliceMM []L

      countStates : ∀ {A} → ∀ pc → AMM A pc → ℕ
      countStates pc []L = zero
      countStates pc (guardMM _ _ x₁ x₂ ∷L x) = suc (countStates _ x)
      countStates pc (branchMM x₁ h h₁ ∷L x) =   countStates _ h ℕ+ countStates _ h₁ ℕ+ countStates _ x


      MMM→NMM-h : ℕ → ∀ pc → MMM pc → NMM pc
      MMM→NMM-h _ _ []L = []L
      MMM→NMM-h k pc (guardMM _ x₁ x₂ y@(withdrawA x₃) ∷L x) = (guardMM k x₁ x₂ y ∷L MMM→NMM-h (suc k) _ x)
      MMM→NMM-h k pc (guardMM _ x₁ x₂ y@(depositA x₃) ∷L x) = (guardMM k x₁ x₂ y ∷L MMM→NMM-h (suc k) _ x)
      MMM→NMM-h k pc (guardMM _ x₁ x₂ y@(publishA x₃ x₄) ∷L x) = (guardMM k x₁ x₂ y ∷L MMM→NMM-h (suc k) _ x)
      MMM→NMM-h k pc (branchMM x₁ x₂ x₃ ∷L x) =
         let b1 = MMM→NMM-h k pc x₂
             b2 = MMM→NMM-h (countStates _ x₂ ℕ+ k) pc x₃
          in (branchMM x₁ b1 b2 ∷L {!MMM→NMM-h (countStates _ x₂ ℕ+ countStates _ x₃) _ x!})
            
      MMM→NMM : ∀ pc → MMM pc → NMM pc
      MMM→NMM = MMM→NMM-h 0



      record StateInfo  : Type₀ where
        field
          stateContext : PublicContext
          action : Action stateContext
          caller : DishonestParticipantId
          


      record ReachFoldState : Type₀ where
        field
          openStates : List ℕ
          statesInfo : List (ℕ × (StateInfo)) 
          computed : ℕ → List (ℕ)

      open ReachFoldState

      initReachFoldState : ReachFoldState
      openStates initReachFoldState = []
      statesInfo initReachFoldState = []
      computed initReachFoldState = const []

      openState : ℕ → StateInfo → ReachFoldState → ReachFoldState
      openStates (openState x y x₁) = x ∷ (openStates x₁) 
      statesInfo (openState x y x₁) = (x , y) ∷ statesInfo x₁
      computed (openState x y x₁) = computed x₁


      
      _isIn_ : ℕ → List ℕ → 𝟚
      x isIn [] = false
      x isIn (x₁ ∷ x₂) = x isIn x₂ or Dec→Bool (discreteℕ x x₁)

      _Li∪_ : List ℕ → List ℕ → List ℕ
      [] Li∪ x₁ = x₁
      (x ∷ xs) Li∪ y =
         Cubical.Data.Bool.if x isIn y
           then xs Li∪ y
           else x ∷ xs Li∪ y

      _Li∪'_ : ∀ {A : Type₀} → List (ℕ × A) → List (ℕ × A) → List (ℕ × A)
      [] Li∪' x₁ = x₁
      (x ∷ xs) Li∪' y =
        Cubical.Data.Bool.if proj₁ x isIn map-List proj₁ y
           then xs Li∪' y
           else x ∷ xs Li∪' y


      registerState : ℕ → ReachFoldState → ReachFoldState
      openStates (registerState x x₁) = (openStates x₁)
      statesInfo (registerState x x₁) = statesInfo x₁ 
      computed (registerState x x₁) k =
        Cubical.Data.Bool.if k isIn (openStates x₁)
         then x ∷ computed x₁ k
         else computed x₁ k


      convergeRFS : ReachFoldState → ReachFoldState → ReachFoldState
      openStates (convergeRFS rfsA rfsB) = (openStates rfsA) Li∪ (openStates rfsB)
      statesInfo (convergeRFS rfsA rfsB) = statesInfo rfsA Li∪' statesInfo rfsB
      computed (convergeRFS rfsA rfsB) k =
          computed rfsA k Li∪ computed rfsB k

      closeAll : ReachFoldState → ReachFoldState
      openStates (closeAll x) = []
      statesInfo (closeAll x) = statesInfo x
      computed (closeAll x) = computed x

      reachability' : ReachFoldState → ∀ pc → NMM pc → ReachFoldState

      reachabilityStep : ReachFoldState → ∀ pc → MM ℕ pc → ReachFoldState
      reachabilityStep rfs pc (guardMM x x₁ x₂ x₃) =
         openState x si (closeAll rfs)
         where
           si : StateInfo
           StateInfo.stateContext si = pc
           StateInfo.action si = x₃
           StateInfo.caller si = x₁
      reachabilityStep rfs pc (branchMM x x₁ x₂) =
         convergeRFS (reachability' rfs pc x₁) (reachability' rfs pc x₂) 


      reachability' rfs pc []L = rfs
      reachability' rfs pc (h ∷L x) = reachability' (reachabilityStep rfs _ h) _ x


      reachability : ∀ pc → NMM pc → ReachFoldState
      reachability = reachability' initReachFoldState




      -- record TranslationReady : Type₀ where
      --   field
      --     numberOfStates : ℕ
      --   StateId = Fin (numberOfStates)

      --   field
      --     stateInfo : StateId → StateInfo
        
          
      --   field
      --     reachableStates : (sId : StateId)  -- → (Γ : Rec (stateContext sId))
      --           → List (StateId × PCExpr (stateContext (stateInfo sId)) Bool)


      -- open StateInfo
      

      -- record TranslationReady : Type₀ where
      --   field
      --     numberOfStates : ℕ
      --   StateId = Fin (numberOfStates)

      --   field
      --     stateInfo : StateId → StateInfo
        
          
      --   field
      --     reachableStates : (sId : StateId)  -- → (Γ : Rec (stateContext sId))
      --           → List (StateId × PCExpr (stateContext (stateInfo sId)) Bool)


      -- open TranslationReady

      -- countStates : ∀ pc → MMM pc → ℕ
      -- countStates pc []L = zero
      -- countStates pc (guardMM _ x₁ x₂ ∷L x) = suc (countStates _ x)
      -- countStates pc (branchMM x₁ h h₁ ∷L x) =   countStates _ h ℕ+ countStates _ h₁ ℕ+ countStates _ x

      -- mkStateInfo : ∀ pc → (x : MMM pc) → Fin (countStates pc x) → StateInfo
      -- mkStateInfo pc []L = empty-rec ∘ ¬Fin0
      -- mkStateInfo pc (guardMM z x₂ x₃ ∷L x) =
      --    sum-rec (λ x₁ → record { stateContext = pc ; action = x₃ ; caller = z })
      --    (λ x₁ → mkStateInfo _ x (fst x₁)) ∘ fsplit
      -- mkStateInfo pc (branchMM x₂ x₃ x₄ ∷L x) x₁ = {!!}

      -- digForStates : {!!}
      -- digForStates = {!!}

      -- mkReachableStates : ∀ pc → (mmm : MMM pc) → (sId : Fin (countStates pc mmm))
      --      → List (Fin (countStates pc mmm) × PCExpr (stateContext (mkStateInfo pc mmm sId)) Bool)
      -- mkReachableStates pc []L = empty-rec ∘ ¬Fin0
      -- mkReachableStates pc (guardMM z x x₁ ∷L mmm) sId = {!!}
      -- mkReachableStates pc (branchMM x x₁ x₂ ∷L mmm) sId = {!!}

      -- MMM'→TR : MMM' → TranslationReady
      -- numberOfStates (MMM'→TR x) = countStates _ x
      -- stateInfo (MMM'→TR x) = mkStateInfo _ x
      -- reachableStates (MMM'→TR x) = {!!}

      -- -- --   OpenStates : Type₀
      -- -- --   OpenStates = List (StateId)

      -- -- -- open TranslationReady


      -- -- -- emptyTranslationReady : TranslationReady
      -- -- -- numberOfStates emptyTranslationReady = zero
      -- -- -- stateInfo emptyTranslationReady = empty-rec ∘ ¬Fin0
      -- -- -- reachableStates emptyTranslationReady _ = []
      
      -- -- -- TRS = Σ TranslationReady OpenStates

      -- -- -- compTRS : List TRS → TRS → TRS
      -- -- -- compTRS hs t = {!!}

      -- -- -- postulate never : Empty 


      -- -- -- {-# TERMINATING #-}
      -- -- -- foldTR : Statements (con [] nothing) → TranslationReady

      -- -- -- foldTRB : ∀ {Τ} → (s : Stmnt (con [] nothing))
      -- -- --           → (bd : Body (bindingMechanics' _ s ) Τ)
      -- -- --           → IsEmpty ⟨ IsPureE (body (AST.bodyR (s ∷L (stmnts bd)) (expr bd))) ⟩ 
      -- -- --           → TRS

      -- -- -- foldTRE : ∀ {Τ} → (e : Expr (con [] nothing) Τ)
      -- -- --           → IsEmpty ⟨ IsPureE e ⟩ 
      -- -- --           → TRS


      -- -- -- foldTR []L = emptyTranslationReady
      -- -- -- foldTR (h ∷L x) with proj₁ (snd (IsPureS h))
      -- -- -- ... | no ¬p = fst (foldTRB h (AST.bodyR x (lit tt)) (¬p ∘ proj₁ ∘ proj₁))
      -- -- -- ... | yes p with h
      -- -- -- ... | AST.bindingS (AST.BS-let ce x₁) =
      -- -- --             let y = (substOneStmnts (inl (evalPureExpr x₁ p)) (mkStatements* x))
      -- -- --             in foldTR y
      -- -- -- ... | AST.nonBindingS x₁ = foldTR x


      -- -- -- foldTRB {Τ} (AST.bindingS (AST.BS-let ce x₁)) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureE x₁)))
      -- -- -- ... | yes p =
      -- -- --    let v = evalPureExpr x₁ p
      -- -- --    in foldTRE ((substOneExpr (inl v) (body bo))) λ _ → never
      -- -- -- ... | no ¬p with (proj₁ (snd (IsPureE (body bo))))
      -- -- -- ... | yes p = {!!}
      -- -- -- ... | no ¬p₁ = {!!}

      -- -- -- foldTRB (AST.bindingS (AST.BS-publish! p (AST.psof name {()}))) (AST.bodyR stmnts₁ expr₁) x₁

      -- -- -- foldTRB s@(AST.nonBindingS y) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureS s))) | (proj₁ (snd (IsPureE (body bo)))) 
      -- -- -- ... | yes p | _ = {!!}
      -- -- -- ... | no ¬p | yes p = {!!}
      -- -- -- ... | no ¬p | no ¬p₁ = {!!}

  
      -- -- -- foldTRE e x = {!!}



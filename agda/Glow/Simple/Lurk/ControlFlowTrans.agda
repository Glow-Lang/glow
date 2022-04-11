{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.ControlFlowTrans where

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

open import Glow.Simple.Lurk.Translation

open import Glow.Simple.Example

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
            {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where


  everyIsDishonest-lem : (ptpsIds : List (Identifier)) → ∀ name →
                           ExistMemberAs (PathP (λ x₁ → Identifier) name)
                                  (map-List proj₁ (map-List (_, false) ptpsIds)) →
                           ExistMemberAs (λ x₁ → (name ≡ proj₁ x₁) × (false ≡ proj₂ x₁))
                                (map-List (_, false) ptpsIds)

  everyIsDishonest-lem [] name ()
  everyIsDishonest-lem (x ∷ ptpsIds) name (inl x₁) = inl (x₁ , refl)
  everyIsDishonest-lem (x ∷ ptpsIds) name (inr x₁) = inr (proj₁ x₁ ∘ proj₁ , (everyIsDishonest-lem ptpsIds name (proj₂ x₁)))


  

  module MonadicControlFlow (ptpsIds : List (Identifier)) (prms : _) (uniquePrms : _) where

    ptps : List (Identifier × ParticipantModality)
    ptps = map-List (_, dishonest) ptpsIds
    
    module MonadicControlFlowUP (uniquePtps : _) where

      ptps'' = ptps

      ih'' : AST.InteractionHead Identifier builtIns one
      ih'' = (AST.interactionHead ptps prms {uniquePrms} {uniquePtps}) 

      open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} ih''

   

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


      FreeExpr = Expr (con [] nothing)

      PID = DishonestParticipantId

      data Action (Γ : Context) : Type₀ where
        withdrawA : (e : Expr Γ Nat) → (⟨ IsPureE e ⟩) → Action Γ
        depositA : (e : Expr Γ Nat) → (⟨ IsPureE e ⟩) → Action Γ 

      _,_⦂_ : Context → Identifier → GType → Context
      Γ , x ⦂ Τ = addToContext Γ (AST.ice nothing x Τ)

      data LMonad (A : Type₀) (Γ : Context) (Τ : GType) : Type₀ where
        action : A → PID → Action Γ → Τ ≡ Unitᵍ → LMonad A Γ Τ
        require : (e : Expr Γ Bool) → (⟨ IsPureE e ⟩) → Τ ≡ Unitᵍ → LMonad A Γ Τ
        expectPub : A → PID → LMonad A Γ Τ
        bind : ∀ {x Τ'} → LMonad A Γ Τ' → LMonad A (Γ , x ⦂ Τ') Τ → LMonad A Γ Τ
        next : ∀ {Τ'} → LMonad A Γ Τ' → LMonad A Γ Τ → LMonad A Γ Τ
        pure : (e : Expr Γ Τ) → (⟨ IsPureE e ⟩) → LMonad A Γ Τ
        branch : (e : Expr Γ Bool) → (⟨ IsPureE e ⟩) → LMonad A Γ Τ → LMonad A Γ Τ → LMonad A Γ Τ
     
      everyIsDishonest : ParticipantId → PID
      everyIsDishonest (AST.pId name {x}) = AST.pId name {everyIsDishonest-lem _ _ x}

      -- TODO : use state monad here
      labelStates : ∀ {A Γ Τ} → ℕ → LMonad A Γ Τ → (Σ ℕ λ _ → LMonad ℕ Γ Τ) 
      labelStates n (action x x₁ x₂ x₃) = (suc n) , (action n x₁ x₂ x₃)
      labelStates n (require e x x₁) = n , (require e x x₁)
      labelStates n (expectPub x x₁) = (suc n) , (expectPub n x₁)
      labelStates n (bind x x₁) =
        let (n₁ , x') = labelStates n x
            (n₂ , x₁') = labelStates n₁ x₁
        in (n₂ , bind x' x₁')
      labelStates n (next x x₁) =
        let (n₁ , x') = labelStates n x
            (n₂ , x₁') = labelStates n₁ x₁
        in (n₂ , next x' x₁')
      labelStates n (pure e x) = n , pure e x
      labelStates n (branch e y x x₁) =
        let (n₁ , x') = labelStates n x
            (n₂ , x₁') = labelStates n₁ x₁
        in (n₂ , (branch e y x' x₁'))

      labelStates' : ∀ {A Γ Τ} → LMonad A Γ Τ → (LMonad ℕ Γ Τ)  
      labelStates' = snd ∘ labelStates 0

      module tryTranslation where
        private       
          _>>=_ = bind-Maybe

        mbIsPureE : ∀ {Γ Τ} → (e : Expr Γ Τ) → Maybe ⟨ IsPureE e ⟩
        mbIsPureE e = mbDec (IsPureE e)

        toLMonadE : ∀ Γ Τ → Expr Γ Τ → Maybe (LMonad Unit Γ Τ)
        toLMonadNBS : ∀ Γ → NBStmnt+Expr Γ → Maybe (LMonad Unit Γ Unitᵍ)

        toLMonadNBS Γ (AST.stmntNBS (AST.NBS-require! x)) =  do
          ispure-x ← mbIsPureE x
          just ((require x ispure-x refl))
        toLMonadNBS Γ (AST.stmntNBS (AST.NBS-deposit! x x₁)) = do
          ispure-x₁ ← mbIsPureE x₁
          just ((action tt (everyIsDishonest x) (depositA x₁ ispure-x₁) refl))
        toLMonadNBS Γ (AST.stmntNBS (AST.NBS-withdraw! x x₁)) = do
          ispure-x₁ ← mbIsPureE x₁
          just ((action tt (everyIsDishonest x) (withdrawA x₁ ispure-x₁) refl))
          -- e ← toLMonadE _ _ x₁ -- TODO make it work for impure exprs!
          -- just (bind e ((action tt (everyIsDishonest x) (withdrawA {!!} {!!}) refl)))
        toLMonadNBS Γ (AST.stmntNBS (AST.NBS-publishVal! x x₁)) = nothing
        toLMonadNBS Γ (AST.exprNBS x) = do
          e ←  (toLMonadE _ _ x) 
          just (next e (pure (lit tt) tt))
        -- toLMonadS : ∀ Γ → Stmnt Γ → Maybe (LMonad Unit Γ Unitᵍ)

        toLMonadE Γ Τ (AST.var x) = just (pure (var x) tt)
        
        toLMonadE Γ Τ (AST.body (AST.bodyR []L expr₁)) = toLMonadE Γ Τ expr₁
        toLMonadE (AST.con entries₁ nothing) Τ (AST.body (AST.bodyR (AST.bindingS (AST.BS-let (AST.ice nothing name type) x) ∷L stmnts₁) expr₁)) = 
          do e' ← toLMonadE _ type x
             es' ← toLMonadE _ _ (body (bodyR stmnts₁ expr₁ ))
             just (bind e' es')
        toLMonadE (AST.con entries₁ (just x₁)) Τ (AST.body (AST.bodyR (AST.bindingS (AST.BS-let (AST.ice nothing name type) x) ∷L stmnts₁) expr₁)) = nothing
        toLMonadE Γ Τ (AST.body (AST.bodyR (AST.bindingS (AST.BS-let (AST.ice (just x₁) name type) x) ∷L stmnts₁) expr₁)) = nothing
        toLMonadE Γ Τ (body (bodyR (bindingS (BS-publish! p x) ∷L stmnts₁) expr₁)) = nothing
        toLMonadE Γ Τ (body (bodyR (nonBindingS x ∷L stmnts₁) expr₁)) =
          do e' ← toLMonadNBS _ x
             es' ← toLMonadE _ _ (body (bodyR stmnts₁ expr₁ ))
             just (next e' es')
        
        toLMonadE Γ Τ (AST.lit x) = just (pure (lit x) tt)
        toLMonadE Γ Τ (x AST.$' x₁) = just (pure (x AST.$' x₁) tt)
        toLMonadE Γ Τ (AST.input x) = nothing
        toLMonadE Γ Τ (AST.sign x) = nothing
        toLMonadE Γ Τ (AST.receivePublished x) = just (expectPub _ x)
        toLMonadE Γ Τ (AST.if e then e₁ else e₂) = do
          ispure-e ← mbIsPureE e
          e₁' ← toLMonadE Γ _ e₁
          e₂' ← toLMonadE Γ _ e₂
          just (branch e ispure-e e₁' e₂')

        toLMonad : ∀ Γ → Statements Γ → Maybe (LMonad Unit Γ Unitᵍ)
        toLMonad Γ x = toLMonadE Γ Unitᵍ (body (AST.bodyR x (lit tt)))

        -- toLMonadS Γ (AST.bindingS (AST.BS-let ce x)) = {!!}
        -- toLMonadS Γ (AST.bindingS (AST.BS-publish! p x)) = nothing
        -- toLMonadS Γ (AST.nonBindingS x) = {!!}

module ToLurkCF (ptpsIds : List (String)) (prms : _) (uniquePrms : _) (uniquePtpnts : _) where

  open import Glow.Simple.ASTDef 


  open AST-String one 

  open MonadicControlFlow.MonadicControlFlowUP {String} {builtIns = Basic-BuiltIns} (ptpsIds) prms uniquePrms (uniquePtpnts) public

  module L = LurkAST List String Unit

  bi-renderer : (Τ : GType) → BI Τ → String
  bi-renderer Τ (AST.bi' bIndex) = bIndex


  module T = Translate.unsafe {String} {builtIns = Basic-BuiltIns} {ptpsIds} {prms} {uniquePrms} {uniquePtpnts}
                tt "cons" bi-renderer
 
  PID→LExpr : PID → L.Expr
  PID→LExpr z = L.ExFieldElem _ (DishonestParticipantId→ℕ ih'' z)
  

  toLurkGlowcode : ∀ {Γ Τ} → LMonad ℕ Γ Τ → L.Expr
  toLurkGlowcode (action x x₁ x₂ Τ≡Unitᵍ) =
     T.appS "action" (L.ExFieldElem _ x ∷ PID→LExpr x₁ ∷ [ h x₂ ])
     where
       h : Action _ → L.Expr
       h (withdrawA x _) = T.appS "withdraw" [ T.translateE x ]
       h (depositA x _) = T.appS "deposit" [ T.translateE x ]

  toLurkGlowcode (expectPub x x₁) = T.appS "publish" (L.ExFieldElem _ x ∷ [ PID→LExpr x₁ ])
  toLurkGlowcode (next x x₁) = T.appS "next" (toLurkGlowcode x ∷ [ toLurkGlowcode x₁ ])
  toLurkGlowcode (bind {x = s} x x₁) =
     T.appS "bind" (toLurkGlowcode x ∷ [ h ])
     where
       h : L.Expr
       h = L.ExLambda _
               [ L.SymbolC s ]
               (toLurkGlowcode x₁)
  toLurkGlowcode (pure x _) = T.appS "mk-pure" [ T.translateE x ]
  toLurkGlowcode (branch x _ x₁ x₂) = L.ExIf _ (T.translateE x) (toLurkGlowcode x₁) (toLurkGlowcode x₂)
  toLurkGlowcode (require e x x₁) = T.appS "require" ([ T.translateE e ])

  module LH = LurkAST AList String Unit


  fixListImp : L.Expr →  LH.Expr
  fixListImp = LurkASTchangeListImp.mapLiImp List AList map-List toAList

  toLurkGlowcode' : ∀ {Γ Τ} → LMonad ℕ Γ Τ → LH.Expr
  toLurkGlowcode' = fixListImp ∘ T.addSignature ∘ toLurkGlowcode

module examplesAB where

  open import Glow.Simple.ASTDef 


  open AST-String one 


  ptps = ("A" ∷ "B" ∷ [])
  uPtps = (toWitness {Q = UniqueByDec≡ proj₁ (map-List (_, false) ("A" ∷ "B" ∷ []))} tt)


  module noParams where
    open ToLurkCF ptps [] tt* uPtps 

    open LH public

    open import Cubical.Data.Unit renaming (tt to TU)

    idA idB : PID
    idA = pId "A" {toWitnessDP ((IsDishonestParticipantId {ptps''} "A")) tt}
    idB = pId "B" {toWitnessDP ((IsDishonestParticipantId {ptps''} "B")) tt}


    testLM : LMonad ℕ (con [] nothing) Unitᵍ 
    testLM = bind {x = "z"} {Τ' = Nat} (next (next (action 0 idA (withdrawA < 2 > tt ) refl)
                        (action 1 idB (depositA < 2 > tt ) refl))
                    (pure < 4 > tt))
                   (bind {x = "zz"} {Τ' = Nat} (expectPub 2 idA)
                       (action 3 idB (withdrawA < 3 > tt) refl))

    testLGC : LH.Expr
    testLGC = (toLurkGlowcode' testLM)


    testOutput : {!!}
    testOutput = {! testLGC!}


  module coinFlip where
    open ToLurkCF ptps ( (AST.iwt "wagerAmount" Nat ∷ AST.iwt "escrowAmount" Nat ∷ []))
                    (toWitness {Q = UniqueByDec≡ IdentifierWithType.name ((AST.iwt "wagerAmount" Nat ∷ AST.iwt "escrowAmount" Nat ∷ []))} tt) uPtps 

    open LH public

    open import Cubical.Data.Unit renaming (tt to TU)


    idA idB : PID
    idA = pId "A" {toWitnessDP ((IsDishonestParticipantId {ptps''} "A")) tt}
    idB = pId "B" {toWitnessDP ((IsDishonestParticipantId {ptps''} "B")) tt}

    testCoinFlip : Statements ih'' (con [] nothing)
    testCoinFlip = coinFlipConsensusCode


    testOutput : LH.Expr
    testOutput = fromJust (map-Maybe (toLurkGlowcode' ∘ labelStates') (tryTranslation.toLMonad _ coinFlipConsensusCode))

    zz = {!testOutput!}

-- module output where
--   open ToLurkCF

--   open LH

--   open import Cubical.Data.Unit renaming (tt to TU)

--   testOutput : {!!}
--   testOutput = {!examplesAB.coinFlip.testOutput!}

-- module ToLurkCF where

--   open import Glow.Simple.ASTDef


--   open AST-String one

--   -- open InteractionHead (AST.interactionHead ptps [] {_} {uniquePtps}) 
--   -- open AST

--   open MonadicControlFlow.MonadicControlFlowUP {String} {builtIns = Basic-BuiltIns} ("A" ∷ "B" ∷ []) {!!} {!!}
--      (toWitness {Q = UniqueByDec≡ proj₁ (map-List (_, false) ("A" ∷ "B" ∷ []))} tt)

--   idA idB : PID
--   idA = pId "A" {toWitnessDP ((IsDishonestParticipantId {ptps''} "A")) tt}
--   idB = pId "B" {toWitnessDP ((IsDishonestParticipantId {ptps''} "B")) tt}
  
--   noBind : ∀ {A Γ Τ} → LMonad A Γ Unitᵍ → LMonad A (Γ , "nobind" ⦂ Unitᵍ) Τ → LMonad A Γ Τ
--   noBind = bind



--   module L = LurkAST List String Unit

--   bi-renderer : (Τ : GType) → BI Τ → String
--   bi-renderer Τ (AST.bi' bIndex) = bIndex


--   module T = Translate.unsafe {String} {builtIns = Basic-BuiltIns} {map-List proj₁ ptps''} {{!!}} {{!!}}
--                 {(toWitness {Q = UniqueByDec≡ proj₁ (map-List (_, false) ("A" ∷ "B" ∷ []))} tt)} 
--                 tt "cons" bi-renderer
 
--   PID→LExpr : PID → L.Expr
--   PID→LExpr z = L.ExFieldElem _ (DishonestParticipantId→ℕ ih'' z)
  

--   toLurkGlowcode : ∀ {Γ Τ} → LMonad ℕ Γ Τ → L.Expr
--   toLurkGlowcode (action x x₁ x₂ Τ≡Unitᵍ) =
--      T.appS "action" (L.ExFieldElem _ x ∷ PID→LExpr x₁ ∷ [ h x₂ ])
--      where
--        h : Action _ → L.Expr
--        h (withdrawA x _) = T.appS "withdraw" [ T.translateE x ]
--        h (depositA x _) = T.appS "deposit" [ T.translateE x ]

--   toLurkGlowcode (expectPub x x₁) = T.appS "publish" (L.ExFieldElem _ x ∷ [ PID→LExpr x₁ ])
--   toLurkGlowcode (next x x₁) = T.appS "next" (toLurkGlowcode x ∷ [ toLurkGlowcode x₁ ])
--   toLurkGlowcode (bind {x = s} x x₁) =
--      T.appS "bind" (toLurkGlowcode x ∷ [ h ])
--      where
--        h : L.Expr
--        h = L.ExLambda _
--                [ L.SymbolC s ]
--                (toLurkGlowcode x₁)
--   toLurkGlowcode (pure x _) = T.appS "mk-pure" [ T.translateE x ]
--   toLurkGlowcode (branch x _ x₁ x₂) = L.ExIf _ (T.translateE x) (toLurkGlowcode x₁) (toLurkGlowcode x₂)
--   toLurkGlowcode (require e x x₁) = T.appS "require" [ T.translateE e ]

--   module LH = LurkAST AList String Unit


--   fixListImp : L.Expr →  LH.Expr
--   fixListImp = LurkASTchangeListImp.mapLiImp List AList map-List toAList


-- -- ----------- some tests/exmaples


-- --   testLM : LMonad ℕ (con [] nothing) Unitᵍ 
-- --   testLM = bind {x = "z"} {Τ' = Nat} (noBind (noBind (action 0 idA (withdrawA < 2 > tt ) refl)
-- --                       (action 1 idB (depositA < 2 > tt ) refl))
-- --                   (pure < 4 > tt))
-- --                  (bind {x = "zz"} {Τ' = Nat} (expectPub 2 idA)
-- --                      (action 3 idB (withdrawA < 3 > tt) refl))

-- --   testLGC : LH.Expr
-- --   testLGC = fixListImp (toLurkGlowcode testLM)



-- --   testCoinFlip : Statements ih'' (((con [] nothing) , "escrowAmount" ⦂ Nat) , "wagerAmount" ⦂ Nat)
-- --   testCoinFlip =
-- --     {!coinFlipConsensusCode!}


-- -- -- :load "/Users/marcin/glow/agda/Glow/Simple/Lurk/host.lurk"
-- -- module output where
-- --   open ToLurkCF

-- --   open LH

-- --   open import Cubical.Data.Unit renaming (tt to TU)

-- --   testOutput : {!!}
-- --   testOutput = {!testLGC!}



-- --       -- data Action (Γ : Context) : Type₀ where
-- --       --   withdrawA : ℕ → PID → Expr Γ Nat → Action Γ
-- --       --   depositA : ℕ → PID → Expr Γ Nat → Action Γ 
-- --       --   -- branching : PCExpr pc Bool → List (Action Γ) → List (Action Γ) → Action Γ

-- --       -- record Actions (Γ : Context) (Τ : GType) : Type₀


-- --       -- data AEnd (Γ : Context) (Τ : GType) : Type₀ where
-- --       --   pureA : ℕ → AEnd Γ Τ
-- --       --   publishA : ℕ → PID → AEnd Γ Τ
-- --       --   letE : {!!} → {!!} →  AEnd Γ Τ
-- --       --   ifA : Expr Γ Bool → Actions Γ Τ → Actions Γ Τ → AEnd Γ Τ
-- --       --   bindA : ∀ Τ' → Actions Γ Τ' → {!!} →  AEnd Γ Τ
 

-- --       -- record Actions Γ Τ where
-- --       --   coinductive
-- --       --   field
-- --       --     actions : List (Action Γ)
-- --       --     aend : AEnd Γ Τ








-- --       --   withdrawA : ℕ → PCExpr pc Nat → Action pc
-- --       --   depositA : ℕ → PCExpr pc Nat → Action pc 
-- --       --   branching : PCExpr pc Bool → List (Action pc) → List (Action pc) → Action pc



-- --       -- LurkIR : PublicContext → Type₀
-- --       -- LurkIR pc = List (Action pc) × Maybe {!LurkIR!}

-- --       -- data Action (pc : PublicContext) : Type₀ where
-- --       --   withdrawA : PCExpr pc Nat → Action pc
-- --       --   depositA : PCExpr pc Nat → Action pc 
-- --       --   publishA : Identifier → GType → Action pc





      



-- --       -- -- AMM : (A : Type₀) → PublicContext →  Type₀ 
-- --       -- -- MMM : PublicContext →  Type₀ 
-- --       -- -- data MM (A : Type₀) (pc : PublicContext) : Type₀ where
-- --       -- --    guardMM : A → DishonestParticipantId → PCExpr pc Bool → Action pc → MM A pc
-- --       -- --    branchMM : PCExpr pc Bool → AMM A pc → AMM A pc → MM A pc






-- --       -- -- -- publicBindingPart : ∀ pc → MM pc → PublicContext
-- --       -- -- -- publicBindingPart pc (guardMM x (withdrawA x₁)) = PC[]
-- --       -- -- -- publicBindingPart pc (guardMM x (depositA x₁)) = PC[]
-- --       -- -- -- publicBindingPart pc (guardMM x (publishA x₁ x₂)) = inl (x₂) PC∷ PC[]
-- --       -- -- -- publicBindingPart pc (branchMM x x₁ x₂) = 
-- --       -- -- --    inr ({!!} , {!!}) PC∷ PC[]

-- --       -- -- {-# TERMINATING #-}
-- --       -- -- publicBinding : ∀ {A} → ∀ pc → MM A pc → PublicContext

-- --       -- -- AMM A = Linked' (publicBinding {A})


-- --       -- -- MMM = Linked' (publicBinding {Unit})

-- --       -- -- NMM = Linked' (publicBinding {ℕ})

-- --       -- -- publicBinding pc (guardMM _ _ x (withdrawA x₁)) = PC[]
-- --       -- -- publicBinding pc (guardMM _ _ x (depositA x₁)) = PC[]
-- --       -- -- publicBinding pc (guardMM _ _ x (publishA x₁ x₂)) = inl (x₂) PC∷ PC[] 
-- --       -- -- publicBinding pc (branchMM x x₁ x₂) =
-- --       -- --    inr (foldLinked' x₁ , foldLinked' x₂) PC∷ PC[]

-- --       -- -- CC : ∀ {_ : Type₀} → Category ℓ-zero ℓ-zero
-- --       -- -- CC {A} = FreeCategory' {C = PublicContext} {A = MM A} publicBinding {!!} {!!}




-- --       -- -- -- testMMM : MMM PC[]
-- --       -- -- -- testMMM = guardMM {!!} (publishA {!!} Nat) ∷L ({!!}  ∷L []L)

-- --       -- -- mkGuard : (ss : Statements (con [] nothing)) → PCExpr (StatementsPC ss) Bool
-- --       -- -- mkGuard = {!!}

-- --       -- -- evalViaPublic : ∀ {Τ} → (ss : Statements (con [] nothing)) → Expr (foldLinked' ss) Τ → PCExpr (StatementsPC ss) Τ 
-- --       -- -- evalViaPublic = {!!}

-- --       -- -- {-# TERMINATING #-}
-- --       -- -- sliceMM : (ss : Statements (con [] nothing))
-- --       -- --           → Statements (foldLinked' ss)
-- --       -- --           → MMM (StatementsPC ss)
-- --       -- -- sliceMM ss []L = []L
-- --       -- -- sliceMM ss (h ∷L []L) = w h
-- --       -- --    where

-- --       -- --     w'' : NBStmnt (foldLinked' ss) → Maybe (Action (StatementsPC ss))
-- --       -- --     w'' (AST.NBS-require! x) = nothing
-- --       -- --     w'' (AST.NBS-deposit! x x₁) = just (depositA (evalViaPublic ss x₁))
-- --       -- --     w'' (AST.NBS-withdraw! x x₁) = just (withdrawA (evalViaPublic ss x₁))
-- --       -- --     w'' (AST.NBS-publishVal! x x₁) = nothing -- TODO: properly handle as imposible! 


-- --       -- --     w : Stmnt (foldLinked' ss) → MMM (StatementsPC ss)
-- --       -- --     w (AST.bindingS (AST.BS-let (AST.ice nothing name type) x)) = w (AST.nonBindingS (AST.exprNBS {!!}))
-- --       -- --     w (AST.bindingS (AST.BS-let (AST.ice (just x₁) name type) x)) = []L -- TODO: properly handle as imposible! 
-- --       -- --     w (AST.bindingS (AST.BS-publish! p x)) = []L -- TODO: properly handle as imposible! 
-- --       -- --     w (AST.nonBindingS (AST.stmntNBS x)) with (w'' x)
-- --       -- --     ... | nothing = []L
-- --       -- --     ... | just x₁ = guardMM _ {!!} (mkGuard ss) x₁ ∷L []L
-- --       -- --     w (AST.nonBindingS (AST.exprNBS x)) = {!x!}
          
-- --       -- -- sliceMM ss (h ∷L (h₁ ∷L x)) =
-- --       -- --    let q = sliceMM ss (h ∷L []L)
-- --       -- --        ss' = Category._⋆_ STMNTS (ss , refl) ((h ∷L []L) , refl)
-- --       -- --        q' = sliceMM (fst ss') (subst (Linked' bindingMechanics') (snd ss') (h₁ ∷L x))
-- --       -- --        z = (Category._⋆_ CC (q , {!!}) (q' , refl ))
-- --       -- --    in fst z

-- --       -- --   where
-- --       -- --     open FreeCategory (publicBinding {A = Unit})
-- --       -- -- -- popMM : Statements (con [] nothing) → Maybe {!!}
-- --       -- -- -- popMM = {!!}

-- --       -- -- MMM' = MMM PC[]

-- --       -- -- mkMM : Statements (con [] nothing) → MMM'
-- --       -- -- mkMM = sliceMM []L

-- --       -- -- countStates : ∀ {A} → ∀ pc → AMM A pc → ℕ
-- --       -- -- countStates pc []L = zero
-- --       -- -- countStates pc (guardMM _ _ x₁ x₂ ∷L x) = suc (countStates _ x)
-- --       -- -- countStates pc (branchMM x₁ h h₁ ∷L x) =   countStates _ h ℕ+ countStates _ h₁ ℕ+ countStates _ x


-- --       -- -- MMM→NMM-h : ℕ → ∀ pc → MMM pc → NMM pc
-- --       -- -- MMM→NMM-h _ _ []L = []L
-- --       -- -- MMM→NMM-h k pc (guardMM _ x₁ x₂ y@(withdrawA x₃) ∷L x) = (guardMM k x₁ x₂ y ∷L MMM→NMM-h (suc k) _ x)
-- --       -- -- MMM→NMM-h k pc (guardMM _ x₁ x₂ y@(depositA x₃) ∷L x) = (guardMM k x₁ x₂ y ∷L MMM→NMM-h (suc k) _ x)
-- --       -- -- MMM→NMM-h k pc (guardMM _ x₁ x₂ y@(publishA x₃ x₄) ∷L x) = (guardMM k x₁ x₂ y ∷L MMM→NMM-h (suc k) _ x)
-- --       -- -- MMM→NMM-h k pc (branchMM x₁ x₂ x₃ ∷L x) =
-- --       -- --    let b1 = MMM→NMM-h k pc x₂
-- --       -- --        b2 = MMM→NMM-h (countStates _ x₂ ℕ+ k) pc x₃
-- --       -- --     in (branchMM x₁ b1 b2 ∷L {!MMM→NMM-h (countStates _ x₂ ℕ+ countStates _ x₃) _ x!})
            
-- --       -- -- MMM→NMM : ∀ pc → MMM pc → NMM pc
-- --       -- -- MMM→NMM = MMM→NMM-h 0



-- --       -- -- record StateInfo  : Type₀ where
-- --       -- --   field
-- --       -- --     stateContext : PublicContext
-- --       -- --     action : Action stateContext
-- --       -- --     caller : DishonestParticipantId
          


-- --       -- -- record ReachFoldState : Type₀ where
-- --       -- --   field
-- --       -- --     openStates : List ℕ
-- --       -- --     statesInfo : List (ℕ × (StateInfo)) 
-- --       -- --     computed : ℕ → List (ℕ)

-- --       -- -- open ReachFoldState

-- --       -- -- initReachFoldState : ReachFoldState
-- --       -- -- openStates initReachFoldState = []
-- --       -- -- statesInfo initReachFoldState = []
-- --       -- -- computed initReachFoldState = const []

-- --       -- -- openState : ℕ → StateInfo → ReachFoldState → ReachFoldState
-- --       -- -- openStates (openState x y x₁) = x ∷ (openStates x₁) 
-- --       -- -- statesInfo (openState x y x₁) = (x , y) ∷ statesInfo x₁
-- --       -- -- computed (openState x y x₁) = computed x₁


      
-- --       -- -- _isIn_ : ℕ → List ℕ → 𝟚
-- --       -- -- x isIn [] = false
-- --       -- -- x isIn (x₁ ∷ x₂) = x isIn x₂ or Dec→Bool (discreteℕ x x₁)

-- --       -- -- _Li∪_ : List ℕ → List ℕ → List ℕ
-- --       -- -- [] Li∪ x₁ = x₁
-- --       -- -- (x ∷ xs) Li∪ y =
-- --       -- --    Cubical.Data.Bool.if x isIn y
-- --       -- --      then xs Li∪ y
-- --       -- --      else x ∷ xs Li∪ y

-- --       -- -- _Li∪'_ : ∀ {A : Type₀} → List (ℕ × A) → List (ℕ × A) → List (ℕ × A)
-- --       -- -- [] Li∪' x₁ = x₁
-- --       -- -- (x ∷ xs) Li∪' y =
-- --       -- --   Cubical.Data.Bool.if proj₁ x isIn map-List proj₁ y
-- --       -- --      then xs Li∪' y
-- --       -- --      else x ∷ xs Li∪' y


-- --       -- -- registerState : ℕ → ReachFoldState → ReachFoldState
-- --       -- -- openStates (registerState x x₁) = (openStates x₁)
-- --       -- -- statesInfo (registerState x x₁) = statesInfo x₁ 
-- --       -- -- computed (registerState x x₁) k =
-- --       -- --   Cubical.Data.Bool.if k isIn (openStates x₁)
-- --       -- --    then x ∷ computed x₁ k
-- --       -- --    else computed x₁ k


-- --       -- -- convergeRFS : ReachFoldState → ReachFoldState → ReachFoldState
-- --       -- -- openStates (convergeRFS rfsA rfsB) = (openStates rfsA) Li∪ (openStates rfsB)
-- --       -- -- statesInfo (convergeRFS rfsA rfsB) = statesInfo rfsA Li∪' statesInfo rfsB
-- --       -- -- computed (convergeRFS rfsA rfsB) k =
-- --       -- --     computed rfsA k Li∪ computed rfsB k

-- --       -- -- closeAll : ReachFoldState → ReachFoldState
-- --       -- -- openStates (closeAll x) = []
-- --       -- -- statesInfo (closeAll x) = statesInfo x
-- --       -- -- computed (closeAll x) = computed x

-- --       -- -- reachability' : ReachFoldState → ∀ pc → NMM pc → ReachFoldState

-- --       -- -- reachabilityStep : ReachFoldState → ∀ pc → MM ℕ pc → ReachFoldState
-- --       -- -- reachabilityStep rfs pc (guardMM x x₁ x₂ x₃) =
-- --       -- --    openState x si (closeAll rfs)
-- --       -- --    where
-- --       -- --      si : StateInfo
-- --       -- --      StateInfo.stateContext si = pc
-- --       -- --      StateInfo.action si = x₃
-- --       -- --      StateInfo.caller si = x₁
-- --       -- -- reachabilityStep rfs pc (branchMM x x₁ x₂) =
-- --       -- --    convergeRFS (reachability' rfs pc x₁) (reachability' rfs pc x₂) 


-- --       -- -- reachability' rfs pc []L = rfs
-- --       -- -- reachability' rfs pc (h ∷L x) = reachability' (reachabilityStep rfs _ h) _ x


-- --       -- -- reachability : ∀ pc → NMM pc → ReachFoldState
-- --       -- -- reachability = reachability' initReachFoldState




-- --       -- -- -- record TranslationReady : Type₀ where
-- --       -- -- --   field
-- --       -- -- --     numberOfStates : ℕ
-- --       -- -- --   StateId = Fin (numberOfStates)

-- --       -- -- --   field
-- --       -- -- --     stateInfo : StateId → StateInfo
        
          
-- --       -- -- --   field
-- --       -- -- --     reachableStates : (sId : StateId)  -- → (Γ : Rec (stateContext sId))
-- --       -- -- --           → List (StateId × PCExpr (stateContext (stateInfo sId)) Bool)


-- --       -- -- -- open StateInfo
      

-- --       -- -- -- record TranslationReady : Type₀ where
-- --       -- -- --   field
-- --       -- -- --     numberOfStates : ℕ
-- --       -- -- --   StateId = Fin (numberOfStates)

-- --       -- -- --   field
-- --       -- -- --     stateInfo : StateId → StateInfo
        
          
-- --       -- -- --   field
-- --       -- -- --     reachableStates : (sId : StateId)  -- → (Γ : Rec (stateContext sId))
-- --       -- -- --           → List (StateId × PCExpr (stateContext (stateInfo sId)) Bool)


-- --       -- -- -- open TranslationReady

-- --       -- -- -- countStates : ∀ pc → MMM pc → ℕ
-- --       -- -- -- countStates pc []L = zero
-- --       -- -- -- countStates pc (guardMM _ x₁ x₂ ∷L x) = suc (countStates _ x)
-- --       -- -- -- countStates pc (branchMM x₁ h h₁ ∷L x) =   countStates _ h ℕ+ countStates _ h₁ ℕ+ countStates _ x

-- --       -- -- -- mkStateInfo : ∀ pc → (x : MMM pc) → Fin (countStates pc x) → StateInfo
-- --       -- -- -- mkStateInfo pc []L = empty-rec ∘ ¬Fin0
-- --       -- -- -- mkStateInfo pc (guardMM z x₂ x₃ ∷L x) =
-- --       -- -- --    sum-rec (λ x₁ → record { stateContext = pc ; action = x₃ ; caller = z })
-- --       -- -- --    (λ x₁ → mkStateInfo _ x (fst x₁)) ∘ fsplit
-- --       -- -- -- mkStateInfo pc (branchMM x₂ x₃ x₄ ∷L x) x₁ = {!!}

-- --       -- -- -- digForStates : {!!}
-- --       -- -- -- digForStates = {!!}

-- --       -- -- -- mkReachableStates : ∀ pc → (mmm : MMM pc) → (sId : Fin (countStates pc mmm))
-- --       -- -- --      → List (Fin (countStates pc mmm) × PCExpr (stateContext (mkStateInfo pc mmm sId)) Bool)
-- --       -- -- -- mkReachableStates pc []L = empty-rec ∘ ¬Fin0
-- --       -- -- -- mkReachableStates pc (guardMM z x x₁ ∷L mmm) sId = {!!}
-- --       -- -- -- mkReachableStates pc (branchMM x x₁ x₂ ∷L mmm) sId = {!!}

-- --       -- -- -- MMM'→TR : MMM' → TranslationReady
-- --       -- -- -- numberOfStates (MMM'→TR x) = countStates _ x
-- --       -- -- -- stateInfo (MMM'→TR x) = mkStateInfo _ x
-- --       -- -- -- reachableStates (MMM'→TR x) = {!!}

-- --       -- -- -- -- --   OpenStates : Type₀
-- --       -- -- -- -- --   OpenStates = List (StateId)

-- --       -- -- -- -- -- open TranslationReady


-- --       -- -- -- -- -- emptyTranslationReady : TranslationReady
-- --       -- -- -- -- -- numberOfStates emptyTranslationReady = zero
-- --       -- -- -- -- -- stateInfo emptyTranslationReady = empty-rec ∘ ¬Fin0
-- --       -- -- -- -- -- reachableStates emptyTranslationReady _ = []
      
-- --       -- -- -- -- -- TRS = Σ TranslationReady OpenStates

-- --       -- -- -- -- -- compTRS : List TRS → TRS → TRS
-- --       -- -- -- -- -- compTRS hs t = {!!}

-- --       -- -- -- -- -- postulate never : Empty 


-- --       -- -- -- -- -- {-# TERMINATING #-}
-- --       -- -- -- -- -- foldTR : Statements (con [] nothing) → TranslationReady

-- --       -- -- -- -- -- foldTRB : ∀ {Τ} → (s : Stmnt (con [] nothing))
-- --       -- -- -- -- --           → (bd : Body (bindingMechanics' _ s ) Τ)
-- --       -- -- -- -- --           → IsEmpty ⟨ IsPureE (body (AST.bodyR (s ∷L (stmnts bd)) (expr bd))) ⟩ 
-- --       -- -- -- -- --           → TRS

-- --       -- -- -- -- -- foldTRE : ∀ {Τ} → (e : Expr (con [] nothing) Τ)
-- --       -- -- -- -- --           → IsEmpty ⟨ IsPureE e ⟩ 
-- --       -- -- -- -- --           → TRS


-- --       -- -- -- -- -- foldTR []L = emptyTranslationReady
-- --       -- -- -- -- -- foldTR (h ∷L x) with proj₁ (snd (IsPureS h))
-- --       -- -- -- -- -- ... | no ¬p = fst (foldTRB h (AST.bodyR x (lit tt)) (¬p ∘ proj₁ ∘ proj₁))
-- --       -- -- -- -- -- ... | yes p with h
-- --       -- -- -- -- -- ... | AST.bindingS (AST.BS-let ce x₁) =
-- --       -- -- -- -- --             let y = (substOneStmnts (inl (evalPureExpr x₁ p)) (mkStatements* x))
-- --       -- -- -- -- --             in foldTR y
-- --       -- -- -- -- -- ... | AST.nonBindingS x₁ = foldTR x


-- --       -- -- -- -- -- foldTRB {Τ} (AST.bindingS (AST.BS-let ce x₁)) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureE x₁)))
-- --       -- -- -- -- -- ... | yes p =
-- --       -- -- -- -- --    let v = evalPureExpr x₁ p
-- --       -- -- -- -- --    in foldTRE ((substOneExpr (inl v) (body bo))) λ _ → never
-- --       -- -- -- -- -- ... | no ¬p with (proj₁ (snd (IsPureE (body bo))))
-- --       -- -- -- -- -- ... | yes p = {!!}
-- --       -- -- -- -- -- ... | no ¬p₁ = {!!}

-- --       -- -- -- -- -- foldTRB (AST.bindingS (AST.BS-publish! p (AST.psof name {()}))) (AST.bodyR stmnts₁ expr₁) x₁

-- --       -- -- -- -- -- foldTRB s@(AST.nonBindingS y) bo@(AST.bodyR stmnts₁ expr₁) xx with (proj₁ (snd (IsPureS s))) | (proj₁ (snd (IsPureE (body bo)))) 
-- --       -- -- -- -- -- ... | yes p | _ = {!!}
-- --       -- -- -- -- -- ... | no ¬p | yes p = {!!}
-- --       -- -- -- -- -- ... | no ¬p | no ¬p₁ = {!!}

  
-- --       -- -- -- -- -- foldTRE e x = {!!}



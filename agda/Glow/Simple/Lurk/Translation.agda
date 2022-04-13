{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.Translation where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
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

open import Cubical.Data.BinNat.BinNat

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
            {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where


  module Translate {ptpsIds : List (Identifier)} {prms : _} {uniqueParams : _} where

    ptps : List (Identifier × ParticipantModality)
    ptps = map-List (_, dishonest) ptpsIds
    
    module safe {uniquePtps : _} where
    
      open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps prms {uniqueParams} {uniquePtps}) 



      open SubstAll {Identifier} {builtIns = builtIns} {ptps = ptps}
      open SubstOne {Identifier} {builtIns = builtIns} {ptps = ptps}

      
      open LurkAST List Identifier Unit renaming (Expr to HExpr)

      -- -- Expr' = Expr (con [] nothing)
      -- {-# TERMINATING #-}
      -- translateE : ∀ {Γ Τ} → (e : Expr Γ Τ) → ⟨ IsPureE e ⟩ → HExpr 
      -- translateE (AST.var (AST.dsot name)) x = ExSymbol _ name
      -- translateE (AST.body (AST.bodyR []L e)) x = translateE e (proj₂ x)
      -- translateE (AST.body (AST.bodyR (h ∷L stmnts) e)) x with h
      -- ... | AST.bindingS (AST.BS-let (AST.ice _ name _) e') =
      --         ExLet _ (record { letBindings = [ record { bInfo = _ ; bKey = name ; bVal = translateE e' _ } ]
      --                         ; letBody = translateE e _ })
      -- ... | AST.bindingS (AST.BS-publish! p x₁) = empty-rec (proj₁ (proj₁ x))
      -- ... | AST.nonBindingS x₁ = translateE (AST.body (AST.bodyR (stmnts) e)) ((proj₂ (proj₁ x)) , (proj₂ x))
      -- translateE (AST.lit x₁) x = {!!}
      -- translateE (x₁ AST.$' x₂) x = ExApply _ {!!} {!!}
      -- translateE (AST.sign x₁) x = {!!}
      -- translateE (AST.if e then e₁ else e₂) x = ExIf _ (translateE e _) (translateE e₁ _) (translateE e₂ _)

      -- translateB : ∀ {Γ Τ} → (b : Body Γ Τ) → ⟨ IsPureB b ⟩ → HExpr
      -- translateB = ?

    module unsafe {uniquePtps : _} {A : Type₀} (a : A) (consI : Identifier)
              (bi-render : ∀ Τ → AST.BI Identifier builtIns one Τ → Identifier) where
    
      open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} (AST.interactionHead ptps prms {uniqueParams} {uniquePtps}) 



      open SubstAll {Identifier} {builtIns = builtIns} {ptps = ptps}
      open SubstOne {Identifier} {builtIns = builtIns} {ptps = ptps}

      
      open LurkAST List Identifier A renaming (Expr to HExpr)


      ni : String → HExpr
      ni x = ExQuotedName a x

      symEx : Identifier → HExpr
      symEx x = ExSymbol a (SymbolC x)

      cons : HExpr → HExpr → HExpr
      cons x x₁ = ExApply a (symEx consI) (x ∷ [ x₁ ])

      appS : Identifier → List HExpr → HExpr
      appS f xs = ExApply a (symEx f ) xs


      translateLit : ∀ Τ → GTypeAgdaRep Τ → HExpr
      translateLit Bool false = ExNil a
      translateLit Bool true = ExT a

      
      translateLit Int x = ni "not-implemented-glow-int-literal"
      translateLit Nat x = h (ℕ→Binℕ x)
        where
          h : Binℕ → HExpr

          h' : Pos → HExpr
          h' (x0 x) = cons (ExNil a) (h' x)
          h' (x1 x) = cons (ExT a) (h x)

          h binℕ0 = ExNil a
          h (binℕpos y) = h' y
      

      translateLit Unitᵍ x = ExQuotedName a "glow-unit-lit"
      
      translateLit Digest x = ni "not-implemented-glow-digest-literal"
      translateLit Signature x = ni "not-implemented-glow-signature-literal"


      addSignature : HExpr → HExpr 
      addSignature = ExLambda a (map-List (λ x → SymbolC (AST.IdentifierWithType.name x)) prms)


      module safeAST where

        translateArg :  ∀ {Γ Τ} → Arg Γ Τ → HExpr
        translateArg (AST.var-a (AST.dsot name)) = ExSymbol a (SymbolC name)
        translateArg {Τ = Τ} (AST.lit-a x) = translateLit _ x

        translateArgs : ∀ {Γ Τ} → Args Γ Τ → Cubical.Data.List.List HExpr
        translateArgs {Τ = []} x = []
        translateArgs {Τ = x₁ ∷ []} x = [ translateArg x ]
        translateArgs {Τ = x₁ ∷ x₂ ∷ Τ} x = translateArg (proj₁ x) ∷ translateArgs (proj₂ x)

        {-# TERMINATING #-}
        -- TODO add proof of purity to arguments
        translateE : ∀ {Γ Τ} → (e : Expr Γ Τ) → HExpr 
        translateE (AST.var (AST.dsot name)) = ExSymbol a (SymbolC name)
        translateE (AST.body (AST.bodyR []L e)) = translateE e 
        translateE (AST.body (AST.bodyR (h ∷L stmnts) e)) with h
        ... | AST.bindingS (AST.BS-let (AST.ice _ name _) e') =
                ExLet a (LetC ( [ (BindingC a (SymbolC name) (translateE e')) ]) (translateE e)) -- (BindingC a name (translateE e'))
        ... | AST.bindingS (AST.BS-publish! p x₁) = ni "fatal-error-publish"
        ... | AST.nonBindingS x₁ = translateE (AST.body (AST.bodyR (stmnts) e))
        translateE (AST.lit x₁) = translateLit _ x₁
        translateE (x₁ AST.$' x₂) = ExApply a (ExSymbol a (SymbolC (bi-render _ x₁))) ( (translateArgs x₂))
        translateE (AST.sign x₁) = ni "fatal-error-sign"
        translateE (AST.if e then e₁ else e₂) = ExIf a (translateE e) (translateE e₁) (translateE e₂)
        translateE (AST.input x) = ni "fatal-error-input"
        translateE (AST.receivePublished x) = ni "fatal-error-recivePub"


      module unsafeAST where

        module U = Unsafe

        translateArg : U.Arg → HExpr
        translateArg (U.var-a name) = ExSymbol a (SymbolC name)
        translateArg (U.lit-a x) = translateLit _ (GlowValue.gValue x)

        translateArgs : U.Args → Cubical.Data.List.List HExpr
        translateArgs = map-List translateArg

        {-# TERMINATING #-}
        -- TODO add proof of purity to arguments
        translateE : (e : U.Expr) → HExpr 
        translateE (U.var _ name) = ExSymbol a (SymbolC name)
        translateE (U.body (U.bodyR [] e)) = translateE e 
        translateE (U.body (U.bodyR (h ∷ stmnts) e)) with h
        ... | U.bindingS (U.BS-let _ name _ e') =
                ExLet a (LetC ( [ (BindingC a (SymbolC name) (translateE e')) ]) (translateE e)) -- (BindingC a name (translateE e'))
        ... | U.bindingS (U.BS-publish! p _ x₁) = ni "fatal-error-publish"
        ... | U.nonBindingS x₁ = translateE (U.body (U.bodyR (stmnts) e))
        translateE (U.lit x) = translateLit _ (GlowValue.gValue x)
        translateE (x₁ U.$' x₂) = ExApply a (ExSymbol a (SymbolC (bi-render _ (AST.bi _  _  _ x₁)) )) ( (translateArgs x₂))
        translateE (U.sign x₁) = ni "fatal-error-sign"
        translateE (U.if e then e₁ else e₂) = ExIf a (translateE e) (translateE e₁) (translateE e₂)
        translateE (U.input t x) = ni "fatal-error-input"
        translateE (U.receivePublished t x) = ni "fatal-error-recivePub"



{-# OPTIONS --cubical --no-import-sorts #-}
module Glow.Meta where

open import Agda.Builtin.String
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List
open import Cubical.Data.Maybe

open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Chain

-- module _ (LanguageId : Type₀)
--          (LanguageVersioning : Type₀) where


record Language : Type₁ where
  field
    SourceCode : Type₀
    IsProgramIn : SourceCode → hProp ℓ-zero

  Validator : Type
  Validator = (source : SourceCode) → ⟨ Decₚ (IsProgramIn source) ⟩

  record Parser {peℓ} {tℓ}
          (ParseError : SourceCode -> Type peℓ)
          (Parsed : Type tℓ) : Type (ℓ-max peℓ tℓ ) where
    
    field
      parse : (source : SourceCode) → (ParseError source) ⊎ (Parsed × (IsEmpty (ParseError source)))
      error-elim : ∀ s → ParseError s → ⟨ ¬ (IsProgramIn s) ⟩
      no-error-elim : ∀ s → (IsEmpty (ParseError s)) → ⟨ IsProgramIn s ⟩

    validate : Validator
    validate source =
      Cubical.Data.Sum.elim
        (λ a → no (error-elim _ a))
        (λ b → yes (no-error-elim _ (proj₂ b)))
        (parse source)

    record Compiler {ceℓ}
           (CompileError : Parsed -> Type ceℓ)
           (Executable : Type tℓ) : Type (ℓ-max ceℓ tℓ ) where

      field
        compile : (p : Parsed) → (CompileError p) ⊎ Executable
        compilationIsSound : ∀ p → IsEmpty (CompileError p)




record ProgramRuntime' {eℓ pvℓ sℓ reℓ epℓ}
                {Executable : Type eℓ}
                {ParametersValue : Executable → Type pvℓ}
                (p : Executable) (pv : ParametersValue p) 
                  :
           Type (ℓ-suc (ℓ-max eℓ (ℓ-max pvℓ (ℓ-max sℓ (ℓ-max reℓ epℓ ) ))) ) where 
  field
    
    RuntimeError : Type reℓ
    State :  Type sℓ    
    initialize :  State
    RuntimeEvent :  State -> Type epℓ
    RuntimeCommand :  State -> Type epℓ
    ExecutionStep : (s : State)  → Type epℓ
    runtimeStep : (s : State) → ExecutionStep s → State 
    executionStep : (s : State)  → (re : RuntimeEvent s)
                       → IsEmpty (ExecutionStep s)
                          ⊎ Σ (ExecutionStep s) λ es → RuntimeCommand (runtimeStep s es) 
    
  module RuntimeChain = Chains State initialize ExecutionStep runtimeStep
  
  field
    terminates : ∀ de → RuntimeChain.FinitePaths de 

    


record Runtime {eℓ pvℓ sℓ reℓ epℓ} : Type (ℓ-suc (ℓ-max eℓ (ℓ-max pvℓ (ℓ-max sℓ (ℓ-max reℓ epℓ ) ))) ) where 
  field
    Executable : Type eℓ
    ParametersValue : Executable → Type pvℓ
    ProgramRuntime :
      (p : Executable) (pv : ParametersValue p)
        → ProgramRuntime' {eℓ} {pvℓ} {sℓ} {reℓ} {epℓ} {Executable} {ParametersValue}
             p pv

  record Invocation : Type ((ℓ-suc (ℓ-max eℓ (ℓ-max pvℓ sℓ)))) where
    field 
      executable : Executable
      paramsVal : ParametersValue executable

    open ProgramRuntime' (ProgramRuntime executable paramsVal) public


  open Invocation public


open Runtime

  -- record RuntimeTrace (p : Executable) (pv : ParametersValue p)
  --          : Type (ℓ-suc (ℓ-max eℓ (ℓ-max pvℓ (ℓ-max sℓ (ℓ-max reℓ epℓ ) ))) ) where
  --   field 
  --     ExecutionTrace : Type epℓ
  --     ExecutionPath : Type epℓ
  --     pushExecutionStep : {et : State p pv}
  --                           → ExecutionStep p pv et
  --                           → ExecutionTrace
  --                           → ExecutionTrace ⊎  ExecutionPath



record EnvironmentModel {ℓ} : Type (ℓ-suc ℓ) where 
  field
    EnvironmentState : Type ℓ
    EnvironmentStateTransition : (es : EnvironmentState) → Type ℓ
    environmentStateTransition : (es : EnvironmentState)
                                → EnvironmentStateTransition es
                                → EnvironmentState  
    EnvironmentEvent : (es : EnvironmentState) → Type ℓ
                                   
  EnvDynamics : Type ℓ
  EnvDynamics = (es : EnvironmentState)
                                → Σ (EnvironmentStateTransition es)
                                    (EnvironmentEvent ∘ environmentStateTransition _ )
    

  record EnvironmentModelInterface : Type (ℓ-suc ℓ) where 
    field
      MsgFromEnvironment : (es : EnvironmentState) → Type ℓ
      MsgToEnvironment : (es : EnvironmentState) → Type ℓ


record InteractionModel {ℓ} (runtime : Runtime {ℓ} {ℓ} {ℓ} {ℓ} {ℓ})
                            (env : EnvironmentModel {ℓ}) : Type (ℓ-suc ℓ) where
  field
    envInterface : EnvironmentModel.EnvironmentModelInterface env

  open EnvironmentModel env


  open EnvironmentModelInterface envInterface

  field
    env→rt : ∀ {es} → {rs : Runtime.Invocation runtime} → ∀ s
               → MsgFromEnvironment es
               → Maybe (Invocation.RuntimeEvent rs s)
    rt→env : ∀ {es} → {rs : Runtime.Invocation runtime} → ∀ s
               → Invocation.RuntimeCommand rs s
               → Maybe (MsgToEnvironment es)
    initialTurn : (rs : Runtime.Invocation runtime)
                       → RuntimeEvent rs (initialize rs)
                          -- ⊎ EnvironmentEvent es
                          
  record WorldInvocation : Type {!!} where
    field
      initEnvState : EnvironmentState
      runtimeInvocation : Runtime.Invocation runtime


    record WorldEdge (rs : ProgramRuntime'.State
          (ProgramRuntime runtime (executable runtimeInvocation)
           (paramsVal runtimeInvocation))) (es : EnvironmentState) : Type {!!} where
      field
        runtimeEvent : RuntimeEvent runtimeInvocation rs
        envEvent : {!EnvironmentEvent !}

    module ExecChain = 
      Chains (State runtimeInvocation × EnvironmentState)
                ((initialize runtimeInvocation) , initEnvState)
                (λ x → WorldEdge (proj₁ x) (proj₂ x))
                {!!}


  -- mkDecidableEdges : EnvDynamics
  --                          → (rs : Runtime.Invocation runtime) → DecidableEdges rs
  -- mkDecidableEdges envDynamics rs node =
  --   let re = {!!}
  --       z = executionStep rs node {!!}
  --   in {!z!}
    
  -- execute : EnvironmentState → EnvDynamics → (rs : Runtime.Invocation runtime) → ChainΣΣ rs
  -- execute envState₀ envDynamics rs = {!!}
    


record LanguageImplementation {ℓ} (language : Language) : Type (ℓ-suc ℓ) where
  field
    ParseError : Language.SourceCode language → Type ℓ
    Parsed : Type ℓ
    parser : Language.Parser language ParseError Parsed
    CompileError : Parsed → Type ℓ
    Executable : Type ℓ
    compiler : Language.Parser.Compiler parser CompileError Executable





record Theory ℓ : Type (ℓ-suc ℓ) where
  field
    language : Language
    implementation : LanguageImplementation {ℓ} language
    environmentModel : EnvironmentModel {ℓ}
    environmentModelInterface : EnvironmentModel.EnvironmentModelInterface environmentModel
    runtime : Runtime {ℓ} {ℓ} {ℓ} {ℓ} {ℓ}
    interactionModel : InteractionModel runtime environmentModel
    
















   -- EnvironmentEvent : (es : EnvironmentState) → Type ℓ
   
   -- EnvironmentStateChange : (es : EnvironmentState) →  Type ℓ




    -- ProgramPredicate : Executable → Type₀
    -- ProgramPredicatePred : (p : Executable) → ProgramPredicate p → Type₀





        
                -- compilationIsSound : {!!}


  -- record Implementation : Type₁ where

  --   field
      

      -- IsProgramIn : SourceCode → hProp ℓ-zero

    



-- record GlowModel : Type₁ where 
--   field
--     Program : Type₀
--     ParametersValue : Program → Type₀
--     State : (p : Program) → ParametersValue p → Type₀
--     RuntimeError : Type₀
--     initialize : (p : Program) → (pv : ParametersValue p) → State p pv
--     ExecutionPath : (p : Program) → ParametersValue p → Type₀
--     ProgramPredicate : Program → Type₀
--     ProgramPredicatePred : (p : Program) → ProgramPredicate p → Type₀





-- record GlowModel : Type₁ where 
--   field
--     Program : Type₀
--     ParametersValue : Program → Type₀
--     State : (p : Program) → ParametersValue p → Type₀
--     RuntimeError : Type₀
--     initialize : (p : Program) → (pv : ParametersValue p) → State p pv
--     ExecutionPath : (p : Program) → ParametersValue p → Type₀
--     ProgramPredicate : Program → Type₀
--     ProgramPredicatePred : (p : Program) → ProgramPredicate p → Type₀




-- record LanguagesFamily ( : Type₀ → Type₀) : Type₁ where
--   constructor language
--   field
--     name : String
--     version : String
--     SourceCode : Type₀


    



    

--     -- initialize : Program → State 
--     -- ExecutionPath : Type₀    

-- -- postulate  : Type₀

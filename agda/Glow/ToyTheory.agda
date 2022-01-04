

{-# OPTIONS --cubical --no-import-sorts #-}
module Glow.ToyTheory where

open import Agda.Builtin.String
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything

open import Cubical.Data.Nat
open import Cubical.Data.Fin
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum
open import Cubical.Data.List
open import Cubical.Data.Maybe
open import Cubical.Data.Bool

open import Cubical.Data.Nat.Order

open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Cubical.Experiments.GlowMeta


ToyLanguage : Language
ToyLanguage =
   record { SourceCode = List Bool
          ; IsProgramIn = λ _ → ⊤ }

ToyImplementation : LanguageImplementation ToyLanguage
ToyImplementation = record
                      { ParseError = λ _ → ⟨ ⊥ ⟩
                      ; Parsed = List Bool
                      ; parser =
                          record { parse = λ source → _⊎_.inr (source , λ x → x)
                                 ; error-elim = λ _ ()
                                 ; no-error-elim = λ s x → tt }
                      ; CompileError = λ _ → ⟨ ⊥ ⟩
                      ; Executable = List Bool
                      ; compiler = record { compile = _⊎_.inr
                                          ; compilationIsSound = λ _ x → x }
                      }


ToyEnvModel : EnvironmentModel
ToyEnvModel = record
                { EnvironmentState = ℕ
                ; EnvironmentStateTransition = λ _ → Unit
                ; environmentStateTransition = λ k _ → suc k
                ; EnvironmentEvent = λ _ → Unit
                ; environmentStep = λ es → tt , tt
                }

lemma-1 : ∀ {j : ℕ} (s : Fin (suc j)) (x : 0 < fst (snd s)) →
       predℕ (fst (snd s)) Cubical.Data.Nat.+ suc (suc (fst s)) ≡
       fst (snd s) Cubical.Data.Nat.+ suc (fst s)
lemma-1 (fst₁ , zero , snd₁) x with ¬-<-zero x
... | ()
lemma-1 (fst₁ , suc fst₂ , snd₁) x = (+-suc fst₂ (suc fst₁))


ToyRuntime : Runtime
ToyRuntime = record
               { Executable = List Bool
               ; ParametersValue = λ _ → Unit
               ; ProgramRuntime =
                   λ p pv → record
                              { RuntimeError = ⟨ ⊥ ⟩ 
                              ; State = Fin (suc (length p)) 
                              ; initialize = fzero
                              ; RuntimeEvent = λ x → 0 < (fst (snd x))  
                              ; RuntimeCommand = λ x → ⟨ ⊤ ⟩ 
                              ; ExecutionStep = λ x → 0 < (fst (snd x))
                              ; runtimeStep =
                                  λ s x → suc (fst s) , (predℕ (fst (snd s)))
                                    ,  lemma-1 s x ∙ snd (snd s)
                              ; executionStep = λ s re → re , tt
                              }
               }


qqqz : ∀ n → Maybe (0 < n)
qqqz zero = nothing
qqqz (suc n) = just (suc-≤-suc zero-≤)

toyInteractionModel : InteractionModel ToyRuntime ToyEnvModel
toyInteractionModel =
  record { envInterface =
             record { MsgFromEnvironment = λ es → ⟨ ⊤ ⟩ 
                    ; MsgToEnvironment = λ es → ⟨ ⊤ ⟩
                    }
         ; env→rt = λ rs x → qqqz _
         ; rt→env = λ rs x → just tt
         }


ToyTheory : Theory ℓ-zero
ToyTheory = record
              { language = ToyLanguage
              ; implementation = ToyImplementation
              ; environmentModel = ToyEnvModel
              ; environmentModelInterface =
                  record { MsgFromEnvironment = λ _ → ⟨ ⊥ ⟩ 
                         ; MsgToEnvironment = λ _ → ⟨ ⊤ ⟩ }
              ; runtime = ToyRuntime
              ; interactionModel = toyInteractionModel
              }

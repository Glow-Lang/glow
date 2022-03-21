
{-# OPTIONS --cubical  #-}
module Glow.Simple.Example where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod renaming (map to map-prod)
open import Cubical.Data.Sum renaming (elim to sum-elim ; map to map-sum)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool hiding (if_then_else_) renaming (Bool to 𝟚 ; _≟_ to _≟B_)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )

open import Cubical.HITs.Interval

-- open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary renaming (¬_ to IsEmpty)
open import Cubical.Relation.Binary

open import Glow.Linked

open import Glow.DecEqMore

open import Glow.Simple.AST

-- open import Glow.Simple.ParamsSubst

open import Glow.Simple.ASTDef



open AST-String zero

-- module AST' = AST-String one

-- someInteraction : Interaction
-- someInteraction =  
--    interaction⟨   "A" ∷ "B" ∷ [] ,  "pI1" ∶ Nat ∷ "b2" ∶ Bool ∷ "b1" ∶ Bool ∷ [] ⟩ (
--         set "x" ∶ Bool ≔ < true > ;
--         at "B" set "y" ∶ Bool ≔ input "enter choice 0" ;
--         publish! "B" ⟶ "y" ;        

--         at "A" set "xx" ∶ Bool ≔
--          -- ( if (bi "and") $' ((var-a (dsot "y")) , var-a (dsot "b2"))
--         ( if (v "y")
--            then
--               (
--               set "z" ∶ Bool ≔ input "enter choice 1" ;₁ ;b
--               v "z"
--             )
--            else (
--             require! v "b2" ;'
--             -- publish! "B" ⟶ "y" ;
--             -- withdraw! "B" ⟵ < 3 > ;
--             -- deposit! "B" ⟶ < 2 > ;
--             set "z" ∶ Bool ≔ < false > ;b
--             < true >
--             )) ;
--         deposit! "B" ⟶ < 2 > ;
--         at "A" set "yq" ∶ Bool ≔ input "enter choice 2" ;
--         withdraw! "A" ⟵ < 3 > ;
--         deposit! "A" ⟶ < 3 > ;
--         -- withdraw! "A" ⟵ < 3 > ;
--         -- withdraw! "B" ⟵ < 3 > ;'
--         publish! "A" ⟶ "xx" ;'        

--         set "yy" ∶ Bool ≔ < false >
--         )

someInteraction : Interaction
someInteraction =  
   interaction⟨   ("A" , honest) ∷ ("B" , honest) ∷ [] , "b2" ∶ Bool ∷  [] ⟩ (
        set "x" ∶ Bool ≔ < true > ;
        at "B" set "y" ∶ Bool ≔ input "enter choice 0" ;
        publish! "B" ⟶ "y" ;        

        at "A" set "xx" ∶ Bool ≔
         ( if (bi "and") $' ((var-a (dsot "y")) , var-a (dsot "b2"))
        -- ( if (v "y")
           then
              (
              set "z" ∶ Bool ≔ input "enter choice 1" ;₁ ;b
              v "z"
            )
           else (
            set "q" ∶ Bool ≔ < false > ;'
            -- require! v "x" ;'
            -- publish! "B" ⟶ "y" ;
            -- withdraw! "B" ⟵ < 3 > ;'
            -- deposit! "B" ⟶ < 2 > ;
            set "z" ∶ Bool ≔  "and" $ (va "q" , va "q") ;b
            < true >
            )) ;
        deposit! "B" ⟶ < 2 > ;
        at "A" set "yq" ∶ Bool ≔ input "enter choice 2" ;
        withdraw! "A" ⟵ < 3 > ;
        deposit! "A" ⟶ < 3 > ;
        -- withdraw! "A" ⟵ < 3 > ;
        -- withdraw! "B" ⟵ < 3 > ;'
        publish! "A" ⟶ "xx" ;'
                
        set "yy" ∶ Bool ≔ < false >
        )





-- someInteraction-proof-demo : Interaction
-- -- toProofs _ _ someInteraction
-- someInteraction-proof-demo = {!!}



-- someInteraction = toProofs {{String-Discrete-postulated}} Test.someInteraction

someCode : Linked'
             (AST.Interaction.bindingMechanics'
              (toProofs String Basic-BuiltIns someInteraction))
             (AST.Interaction.emptyContext
              (toProofs String Basic-BuiltIns someInteraction)) 
someCode = AST.Interaction.code (toProofs _ _ someInteraction) 



-- -- someInteraction' : AST.Interaction String one
-- -- AST.Interaction.head someInteraction' = transp (λ i → AST.InteractionHead String (seg i)) i0
-- --                                           (AST.Interaction.head Test.someInteraction)
-- -- AST.Interaction.code someInteraction' = {!AST.Interaction.code (toProofs Test.someInteraction)!}

  

-- -- module Test' where
-- --   open AST String {{String-Discrete-postulated}}

-- --   trnsprt : Interaction zero  →
-- --                 Interaction one
-- --   trnsprt = transport λ i → Interaction (seg i)


-- --   trsnprtTest = trnsprt Test.someInteraction

-- --   trsnprtTest' : Maybe GType
-- --   trsnprtTest' = map-Maybe ContextEntry'.type  ( (safeHead (Context.entries (foldLinked' (Interaction.code trsnprtTest)))))

-- --   trsnprtTest'' : (("pI1" ≡ "b2") ⊎
-- --                      (("pI1" ≡ "b2" → Empty) ×
-- --                       (("pI1" ≡ "b1") ⊎ (("pI1" ≡ "b1" → Empty) × Lift Empty))) →
-- --                      Empty)
-- --                     ×
-- --                     (("b2" ≡ "b1") ⊎ (("b2" ≡ "b1" → Empty) × Lift Empty) → Empty) ×
-- --                     (Lift Empty → Empty) × Lift Unit
-- --   trsnprtTest'' = InteractionHead.uniqueParams (Interaction.head trsnprtTest)

-- --   trsnprtTest''* : {!!} 
-- --   trsnprtTest''* = InteractionHead.uniqueParams (Interaction.head Test.someInteraction)

-- --   -- trsnprtTest''' = {!!}


-- -- module Testℕ where 
-- --   open AST ℕ zero

-- --   someInteraction : Interaction
-- --   someInteraction =  
-- --      interaction⟨   1 ∷ 2 ∷ [] ,  3 ∶ Nat ∷ 4 ∶ Bool ∷ 5 ∶ Bool ∷ [] ⟩ (
-- --           set 6 ∶ Bool ≔ < true > ;
-- --           at 2 set 7 ∶ Bool ≔ v 5 ;
-- --           at 1 set 8 ∶ Bool ≔ (
-- --               require! v 4 ;'
-- --               -- publish! "B" ⟶ "y" ;
-- --               -- withdraw! "B" ⟵ < 3 > ;
-- --               -- deposit! "B" ⟶ < 2 > ;
-- --               set 9 ∶ Bool ≔ < false > ;b
-- --               < true >
-- --               );
-- --           deposit! 2 ⟶ < 2 > ;
-- --           withdraw! 2 ⟵ < 3 > ;
-- --           publish! 2 ⟶ 7 ;'        
-- --           set 10 ∶ Bool ≔ v 7 )


-- -- module Testℕ' where
-- --   open AST ℕ

-- --   trnsprt : Interaction zero  →
-- --                 Interaction one
-- --   trnsprt = transport λ i → Interaction (seg i)


-- --   trsnprtTest = trnsprt Testℕ.someInteraction

-- --   trsnprtTest' : Maybe GType
-- --   trsnprtTest' = map-Maybe ContextEntry'.type  ( (safeHead (Context.entries (foldLinked' (Interaction.code trsnprtTest)))))

-- --   trsnprtTest'' : PropMode.PM one
-- --                     (UniqueBy
-- --                      (λ x x₁ →
-- --                         AST.IdentifierWithType.name x ≡ AST.IdentifierWithType.name x₁)
-- --                      (AST.parameters (AST.Interaction.head trsnprtTest))
-- --                      ,
-- --                      (UniqueByDec≡ AST.IdentifierWithType.name
-- --                       (AST.parameters (AST.Interaction.head trsnprtTest))
-- --                       ,
-- --                       isProp-UniqueBy
-- --                       (λ x x₁ →
-- --                          AST.IdentifierWithType.name x ≡ AST.IdentifierWithType.name x₁)
-- --                       (AST.parameters (AST.Interaction.head trsnprtTest))))
-- --   trsnprtTest'' = InteractionHead.uniqueParams (Interaction.head trsnprtTest)


-- --   -- trsnprtTest''' : {!!}
-- --   -- trsnprtTest''' = (λ { (inl p)
-- --   --                           → transp
-- --   --                             (λ i → caseNat ℕ Empty (predℕ (predℕ (predℕ (proj₁ p i))))) i0 0
-- --   --                       ; (inr q)
-- --   --                           → Cubical.Data.Sum.Base..extendedlambda0
-- --   --                             (no
-- --   --                              (λ x →
-- --   --                                 transp (λ i → caseNat ℕ Empty (predℕ (predℕ (predℕ (proj₁ x i)))))
-- --   --                                 i0 0))
-- --   --                             (no (λ x → lower (proj₂ x)))
-- --   --                             (λ x →
-- --   --                                transp (λ i → caseNat ℕ Empty (predℕ (predℕ (predℕ (proj₁ x i)))))
-- --   --                                i0 0)
-- --   --                             (λ x → lower (proj₂ x)) (proj₂ q)
-- --   --                       })
-- --   --                    ,
-- --   --                    ((λ { (inl p)
-- --   --                            → transp
-- --   --                              (λ i → caseNat ℕ Empty (predℕ (predℕ (predℕ (predℕ (proj₁ p i))))))
-- --   --                              i0 0
-- --   --                        ; (inr q) → lower (proj₂ q)
-- --   --                        })
-- --   --                     , (lower , tt*))

-- --   xtx : PropMode.PM one
-- --                     (UniqueBy
-- --                      (λ x x₁ →
-- --                         AST.IdentifierWithType.name x ≡ AST.IdentifierWithType.name x₁)
-- --                      (AST.parameters (AST.Interaction.head trsnprtTest))
-- --                      ,
-- --                      (UniqueByDec≡ AST.IdentifierWithType.name
-- --                       (AST.parameters (AST.Interaction.head trsnprtTest))
-- --                       ,
-- --                       isProp-UniqueBy
-- --                       (λ x x₁ →
-- --                          AST.IdentifierWithType.name x ≡ AST.IdentifierWithType.name x₁)
-- --                       (AST.parameters (AST.Interaction.head trsnprtTest))))
-- --   xtx = {!!} , ({!!} , ({!!} , {!tt*!}))

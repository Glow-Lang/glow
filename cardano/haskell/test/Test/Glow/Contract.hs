{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Glow.Contract where

import Glow.Contract (getCodeAndExitPoint, transition, validateEntryPoint)
import qualified Glow.Types as Glow -- necessary for some Glow types / constructors which shadow Plutus names
import qualified PlutusTx.AssocMap as Map
import qualified Test.Glow.Samples as Samples
import Test.Glow.Utils (assertTransitStateEq)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

contractTests :: TestTree
contractTests =
  testGroup
    "contract"
    [ initialMoveTransitionTest,
      getCodeAndExitPointTest,
      validateEntryPointTest,
      setParticipantTest,
      expectDepositTest
    ]

validateEntryPointTest :: TestTree
validateEntryPointTest = testCase "initial move entrypoint validation" $ do
  assertEqual
    "Should validate"
    (Just ())
    (validateEntryPoint Samples.initialContractEntryPoint Samples.initialParamsEntryPoint)

getCodeAndExitPointTest :: TestTree
getCodeAndExitPointTest = testCase "initial move get code and exitpoint" $ do
  assertEqual
    "Should get code and exitpoint"
    (Just (Samples.buySigInitialCode, Samples.buySigInitialExitPoint))
    (getCodeAndExitPoint Samples.initialParamsEntryPoint Samples.buySigGlowContract)

initialMoveTransitionTest :: TestTree
initialMoveTransitionTest = testCase "initial move transition" $ do
  assertTransitStateEq transitionedStateResult finalState
  where
    transitionedStateResult =
      transition
        Samples.glowConfig
        Samples.buySigInitialState
        Samples.buySigInitialRedeemer
    finalState = Samples.buySigTransitionState1

setParticipantTest :: TestTree
setParticipantTest = testCase "initial move set participant" $ do
  assertEqual
    "Should be able to lookup participant"
    (Just $ Glow.PubKey Samples.buyerPubKey)
    (Map.lookup "Buyer" Samples.buySigInitialVarMap)

expectDepositTest :: TestTree
expectDepositTest = testCase "initial move set participant" $ do
  assertEqual
    "Should be able to lookup price"
    (Just $ Glow.Integer Samples.buySigInitialPrice)
    (Map.lookup "price" Samples.buySigInitialVarMap)

-- test addToValue?

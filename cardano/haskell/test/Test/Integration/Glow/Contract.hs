{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Integration.Glow.Contract where

import Data.Default (def)
import qualified Data.Map as Map
import Glow.Client
import Glow.Client.Types (RawCreateParams (..), RawMoveParams (..))
import Glow.Types hiding (Signature)
import qualified Ledger.Ada as Ada
import Ledger.Value (Value (..))
import Plutus.Contract.Test
  ( Wallet (..),
    assertDone,
    checkPredicateOptions,
    defaultCheckOptions,
    emulatorConfig,
  )
import Plutus.Trace.Emulator (EmulatorConfig (..), EmulatorTrace)
import qualified Plutus.Trace.Emulator as Emulator
import PlutusPrelude (void, (&), (.~))
import qualified Test.Glow.Samples as Samples
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

contractIntegrationTests :: TestTree
contractIntegrationTests =
  testGroup
    "contract"
    [ testBuySigEnd
    , testCase "emulate buySig" emulateBuySig
    , testCase "emulate buySig with waiting" emulateBuySigWait
    ]

testBuySigEnd :: TestTree
testBuySigEnd =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ testContractCfg)
    "successful buy signature run"
    (assertDone testContract sellerInst (const True) "contract should close")
    -- TODO: how much is left in wallet
    -- TODO: (assertNotDone testContract buyerInst (const True) "contract should close")
    traceBuySignature
  where
    testContract = glowContract Samples.glowConfig
    -- buyerInst  = Emulator.walletInstanceTag Samples.buyer
    sellerInst = Emulator.walletInstanceTag Samples.seller

emulateBuySig :: IO ()
emulateBuySig = Emulator.runEmulatorTraceIO' def testContractCfg traceBuySignature

emulateBuySigWait :: IO ()
emulateBuySigWait = Emulator.runEmulatorTraceIO' def testContractCfg traceBuySigWait

testContractCfg :: EmulatorConfig
testContractCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet w, v) | w <- [1, 2]]
  where
    v :: Value
    v = Ada.lovelaceValueOf 1000_000_000_000_000

-- NOTE: glowContract does not refer to on-chain glow code,
-- but rather the Plutus contract glow uses to run it.
traceBuySignature :: EmulatorTrace ()
traceBuySignature = do
  buyerH <- Emulator.activateContractWallet Samples.buyer $ glowContract $ GlowConfig defaultTimeoutLength
  sellerH <- Emulator.activateContractWallet Samples.seller $ glowContract $ GlowConfig defaultTimeoutLength

  Emulator.callEndpoint @"wait" sellerH ()

  Emulator.callEndpoint @"create" buyerH (RawCreateParams Samples.contractSource Samples.initialBuySigVarMap 100)
  void $ Emulator.waitNSlots 2 -- TODO: how long to wait? ANS: for some reason 1 fails
  -- FIXME: we need to watch for events eventually...?
  Emulator.callEndpoint @"move" sellerH (RawMoveParams Samples.sellerBuySigVarMap "cp0")
  void $ Emulator.waitNSlots 1

-- emulateBuySigCreate :: EmulatorTrace ()
-- emulateBuySigCreate = do
--   buyerH <- Emulator.activateContractWallet Samples.buyer $ glowContract $ GlowConfig defaultTimeoutLength
--   sellerH <- Emulator.activateContractWallet Samples.seller $ glowContract $ GlowConfig defaultTimeoutLength

--   Emulator.callEndpoint @"create" buyerH (RawCreateParams Samples.contractSource Samples.initialBuySigVarMap 100)
--   Emulator.callEndpoint @"create" sellerH (RawCreateParams Samples.contractSource Samples.initialBuySigVarMap 100)

traceBuySigWait :: EmulatorTrace ()
traceBuySigWait = do
  buyerH <- Emulator.activateContractWallet Samples.buyer $ glowContract $ GlowConfig defaultTimeoutLength
  sellerH <- Emulator.activateContractWallet Samples.seller $ glowContract $ GlowConfig defaultTimeoutLength

  -- Seller: Wait for buyer to start the contract
  Emulator.callEndpoint @"wait" sellerH ()
  void $ Emulator.waitNSlots 1

  -- Buyer: Create the contract
  Emulator.callEndpoint @"create" buyerH (RawCreateParams Samples.contractSource Samples.initialBuySigVarMap 100)
  void $ Emulator.waitNSlots 2

  -- Buyer: Wait for seller to act
  Emulator.callEndpoint @"wait" buyerH ()
  void $ Emulator.waitNSlots 1

  -- Seller: Sees the tracelog, seller acts
  Emulator.callEndpoint @"move" sellerH (RawMoveParams Samples.sellerBuySigVarMap "cp0")
  void $ Emulator.waitNSlots 2
  -- END

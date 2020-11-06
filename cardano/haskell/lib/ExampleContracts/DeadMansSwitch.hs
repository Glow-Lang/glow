{--

#lang glow
// -*- JavaScript -*-
// Inspired by https://github.com/KarolTrzeszczkowski/Electron-Cash-Last-Will-Plugin

data Command = Withdraw(x : nat) | Inherit(x : nat)

@interaction([Owner, Heir])
let deadManSwitch = (expirationDelay) => {
   let rec loop = (expirationTimestamp) =>
     choice {
     | @_ deposit! x ;
       loop (expirationTimestamp);
     | @Owner publish! Withdraw(x);
       withdraw! Owner <- x ;
       loop (now() + expirationDelay);
     | @Heir publish! Inherit(x);
       require! now() >= expirationTimestamp;
       withdraw! Heir <- x;
       loop (expirationTimestamp); };
   loop(now() + expirationDelay); }

--}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}

module ExampleContracts.DeadMansSwitch where

import Data.Aeson hiding (Value)
import GHC.Generics

import Language.PlutusTx.Prelude
import Ledger (pubKeyHash, Datum (..), TxInInfo (..), TxOutType (..), ValidatorCtx (..), TxInfo (..), TxOut (..), findDatumHash, findOwnInput, txSignedBy, getContinuingOutputs)
import Ledger.Value (lt)
import Ledger.Crypto (PubKeyHash)
import qualified Ledger.Interval as Interval
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Slot (Slot (..))
import Ledger.Typed.Scripts (ScriptType)
import Ledger.Typed.Scripts.Validators (RedeemerType, DatumType)
import qualified Language.PlutusTx as PlutusTx
import Wallet.Emulator (Wallet (..), walletPubKey)
import Language.PlutusCore.Universe

data Config = Config
  { ownerAddress :: PubKeyHash
  , heirAddress :: PubKeyHash
  , expirationDelay :: Integer
  } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Config

defaultConfig :: Config
defaultConfig = Config
  { ownerAddress = pubKeyHash $ walletPubKey (Wallet 1)
  , heirAddress = pubKeyHash $ walletPubKey (Wallet 2)
  , expirationDelay = 60
  }

data DeadMansSwitch
instance ScriptType DeadMansSwitch where
  type instance RedeemerType DeadMansSwitch = Command
  type instance DatumType DeadMansSwitch = Slot

data Command
  = Deposit
  | Withdraw
  | Inherit

PlutusTx.makeIsData ''Command
PlutusTx.makeLift ''Command

validate :: Config -> Slot -> Command -> ValidatorCtx -> Bool
validate Config{ownerAddress, heirAddress, expirationDelay} expirationSlot command ctx =
  -- TODO: What if there are two inputs with the same script?
  case getContinuingOutputs ctx of
    [output] ->
      case command of
        Deposit ->
             inputValue `lt` outputValue
          && outputsState expirationSlot output
          where
            inputValue = txInInfoValue (findOwnInput ctx)
            outputValue = txOutValue output

        -- TODO: Add lock for refreshing.
        Withdraw ->
             isSignedBy ownerAddress
          && outputsState (currentSlot + Slot expirationDelay) output

        -- TODO: Also add lock for inheriting? Or require that the entire amount be withdrawn.
        Inherit ->
             isSignedBy heirAddress
          && Interval.after expirationSlot (txInfoValidRange (valCtxTxInfo ctx))
          && outputsState expirationSlot output

    _ -> False

  where
    isSignedBy address =
      valCtxTxInfo ctx `txSignedBy` address

    outputsState datum output =
      case output of
        TxOut{txOutType=PayToScript svh} ->
          datumHash datum == Just svh
        _ -> False

    -- TODO: Find a more robust way to calculate the current slot. Is upper bound better than lower bound?
    -- What happens if it is unbounded in either direction?
    currentSlot = slotNumber
      where Interval.UpperBound (Interval.Finite slotNumber) _ = Interval.ivTo (txInfoValidRange (valCtxTxInfo ctx))

    datumHash datum = findDatumHash (Datum (PlutusTx.toData datum)) (valCtxTxInfo ctx)

wrappedScript :: Scripts.ScriptInstance DeadMansSwitch
wrappedScript = Scripts.validator @DeadMansSwitch
    ($$(PlutusTx.compile [|| validate ||]) `PlutusTx.applyCode` PlutusTx.liftCode defaultConfig)
    $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @Slot @Command

unwrappedScript :: PlutusTx.CompiledCode DefaultUni (Slot -> Command -> ValidatorCtx -> Bool)
unwrappedScript =
  $$(PlutusTx.compile [|| validate ||]) `PlutusTx.applyCode` PlutusTx.liftCode defaultConfig

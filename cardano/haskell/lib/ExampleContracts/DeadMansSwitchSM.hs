-- Another implementation of the dead man's switch contract, this time using Cardano's state machine libraries.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}

module ExampleContracts.DeadMansSwitchSM where

import Data.Aeson hiding (Value)
import GHC.Generics

import Language.PlutusTx.Prelude
import Ledger (ValidatorCtx (..), TxInfo (..))
import Ledger.Value (Value (..))
import Ledger.Constraints.TxConstraints (TxConstraints (..), mustBeSignedBy)
import Ledger.Crypto (PubKeyHash)
import qualified Ledger.Interval as Interval
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Slot (Slot (..))
import Ledger.Typed.Scripts (ScriptType)
import Ledger.Typed.Scripts.Validators (RedeemerType, DatumType)
import qualified Language.PlutusTx as PlutusTx
import Language.Plutus.Contract.StateMachine (State (..), Void)
import qualified Language.Plutus.Contract.StateMachine as SM

data Config = Config
  { ownerAddress :: PubKeyHash
  , heirAddress :: PubKeyHash
  , expirationDelay :: Integer
  } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Config

data DeadMansSwitch
instance ScriptType DeadMansSwitch where
  type instance RedeemerType DeadMansSwitch = Command
  type instance DatumType DeadMansSwitch = Slot

-- Unlike the other implementation, for the state machine the redeemer data also needs to carry a currency
-- value to calculate a new currency value for the state in the transition function. If the transition function
-- had access to the transaction outputs or if constraint on the the currency value was optional, then this
-- wouldn't be an issue.
data Command
  = Deposit Value
  | Withdraw Slot Value -- Also needs to carry current slot number to compute state transition, which is annoying.
  | Inherit Value

PlutusTx.makeIsData ''Command
PlutusTx.makeLift ''Command

transition :: Config -> State Slot -> Command -> Maybe (TxConstraints Void Void, State Slot)
transition Config{ownerAddress, heirAddress, expirationDelay} State{stateData=oldData,stateValue} input =
  case input of
    Deposit amount ->
      Just ( mempty
           , State { stateData = oldData, stateValue = stateValue <> amount }
           )
    Withdraw currentSlot amount ->
      Just ( mustBeSignedBy ownerAddress
           , State { stateData = currentSlot + Slot expirationDelay, stateValue = stateValue <> inv amount }
           )
    Inherit amount ->
      Just ( mustBeSignedBy heirAddress
           , State { stateData = oldData, stateValue = stateValue <> inv amount }
           )

machine :: Config -> SM.StateMachine Slot Command
machine cfg = SM.StateMachine
  { SM.smTransition = transition cfg
  , SM.smFinal = const False
  , SM.smCheck = \state input ctx ->
    case input of
      Deposit _ -> True
      Withdraw _ _ -> True
      Inherit _ -> Interval.after state $ txInfoValidRange (valCtxTxInfo ctx)
  }

mkValidator :: Config -> Scripts.ValidatorType (SM.StateMachine Slot Command)
mkValidator cfg = SM.mkValidator $ machine cfg

scriptInstance :: Config -> Scripts.ScriptInstance (SM.StateMachine Slot Command)
scriptInstance config = Scripts.validator @(SM.StateMachine Slot Command)
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode config)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Slot @Command
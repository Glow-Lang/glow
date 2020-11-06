{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Client where

import Control.Lens
import Control.Monad (void)
import Data.Aeson hiding (Value)
import qualified Data.ByteString.Char8 as BSC
import Debug.Trace
import GHC.Generics

import Language.Plutus.Contract
import qualified Language.Plutus.Contract.StateMachine as SM
import qualified Language.PlutusTx.AssocMap as Map
import qualified Ledger as Ledger
import Ledger.Typed.Tx

import Contract
import Types

data CreateParams = CreateParams
  { datatypes :: DatatypeMap
  , participants :: [Ledger.PubKey]
  , arguments :: VariableMap
  , contract :: GlowContract
  , timeoutLength :: Integer
  } deriving (Generic, Show, FromJSON, ToJSON)

data MoveParams = MoveParams
  { variableMap :: VariableMap
  , entryPoint :: String
  } deriving (Generic, Show, FromJSON, ToJSON)

type GlowSchema =
  BlockchainActions
    .\/ Endpoint "create" CreateParams
    .\/ Endpoint "move" MoveParams
    .\/ Endpoint "wait" ()

data GlowError
  = StateMachineError (SM.SMContractError GlowDatum GlowRedeemer)
  | OtherContractError ContractError
  deriving (Show)

makeClassyPrisms ''GlowError

instance SM.AsSMContractError GlowError GlowDatum GlowRedeemer where
    _SMContractError = _StateMachineError

instance AsContractError GlowError where
    _ContractError = _OtherContractError

glowContract :: GlowConfig -> Contract GlowSchema GlowError ()
glowContract cfg =
  create `select` wait
  where
    create = do
      traceLog "activating create endpoint"
      params <- endpoint @"create" @CreateParams @GlowSchema
      traceLog "creating contract"
      createContract cfg params
      traceLog "contract created"
      move `select` wait

    move = do
      traceLog "activating move endpoint"
      params <- endpoint @"move" @MoveParams @GlowSchema
      traceLog "executing move"
      output <- executeMove cfg params
      case gdExecutionPoint output of
        Just _ -> do
          traceLog "move succeeded"
          move `select` wait
        Nothing ->
          traceLog "move succeeded and hit end state"

    wait = do
      traceLog "activating wait endpoint"
      () <- endpoint @"wait" @() @GlowSchema
      traceLog "waiting for update"
      update <- SM.waitForUpdate $ glowClient cfg
      case update of
        Just (TypedScriptTxOut{tyTxOutData=_state}, _txOutRef) -> do
          traceLog "contract updated, finished waiting"
          move `select` wait
        Nothing ->
          pure ()

createContract
  :: GlowConfig -> CreateParams -> Contract GlowSchema GlowError ()
createContract cfg params = do
  traceLog $ "create params: " <> show params
  let datum = GlowDatum
          { gdContract = contract params
          , gdVariableMap = arguments params
          , gdFunctionMap = Map.empty
          , gdDatatypeMap = datatypes params
          , gdExecutionPoint = Just "begin"
          , gdDeadline = Ledger.Slot (timeoutLength params)
          }
  traceLog $ show datum
  void $ awaitSlot 0
  let payValue = mkValue 0
  traceLog "initializing contract"
  void $ SM.runInitialise (glowClient cfg) datum payValue
  traceLog "contract initialized"
  traceLog "executing setup move"
  void $ executeMove cfg (MoveParams Map.empty "begin")
  traceLog "setup move executed"

executeMove
  :: GlowConfig -> MoveParams -> Contract GlowSchema GlowError GlowDatum
executeMove cfg params = do
  traceLog $ "move params: " <> show params
  datum <- SM.runStep (glowClient cfg) (BSC.pack (entryPoint params), variableMap params)
  traceLog $ show datum
  return datum

glowClient :: GlowConfig -> SM.StateMachineClient GlowDatum GlowRedeemer
glowClient cfg = SM.mkStateMachineClient $
  SM.StateMachineInstance (machine cfg) (scriptInstance cfg)

traceLog :: String -> Contract s e ()
traceLog msg =
  void $ trace msg $ logInfo @String msg
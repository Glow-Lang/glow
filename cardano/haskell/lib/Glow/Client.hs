{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Glow.Client where

import Control.Lens
import Control.Monad (void)
import Data.Aeson hiding (Value)
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import Debug.Trace
import GHC.Generics
import Glow.Client.Types
  ( CreateParams (..),
    MoveParams (..),
    RawCreateParams (..),
    RawMoveParams (..),
    SExprString (..),
  )
import Glow.Contract
import Glow.Parser (parseRawCreateParams, parseRawMoveParams)
import Glow.Types
import qualified Ledger as Ledger
import Ledger.Typed.Tx
import Plutus.Contract
import qualified Plutus.Contract.StateMachine as SM
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map
import Schema (ToArgument, ToSchema)

type GlowSchema =
  Endpoint "create" RawCreateParams
    .\/ Endpoint "move" RawMoveParams
    .\/ Endpoint "wait" ()

data GlowError
  = -- TODO: Is this lossy? GlowDatum, GlowRedeemer removed.
    -- = StateMachineError (SM.SMContractError GlowDatum GlowRedeemer)
    StateMachineError SM.SMContractError
  | OtherContractError ContractError
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

makeClassyPrisms ''GlowError

-- TODO: Is this lossy? GlowDatum, GlowRedeemer removed.
-- instance SM.AsSMContractError GlowError GlowDatum GlowRedeemer where
instance SM.AsSMContractError GlowError where
  _SMContractError = _StateMachineError

instance AsContractError GlowError where
  _ContractError = _OtherContractError

glowContract :: GlowConfig -> Contract () GlowSchema GlowError ()
glowContract cfg =
  create `select` wait
  where
    -- Deploy our glow code to Cardano Blockchain
    create = do
      traceLog "activating create endpoint"
      rawCreateParams <- endpoint @"create" @RawCreateParams
      let createParams = parseRawCreateParams rawCreateParams
      traceLog "creating contract"
      createContract cfg createParams
      traceLog "contract created"
      move `select` wait

    -- Execute interaction step
    move = do
      traceLog "activating move endpoint"
      rawMoveParams <- endpoint @"move" @RawMoveParams
      let moveParams = parseRawMoveParams rawMoveParams
      traceLog "executing move"
      output <- executeMove cfg moveParams
      case gdExecutionPoint output of
        Just _ -> do
          traceLog "move succeeded"
          move `select` wait
        Nothing ->
          traceLog "move succeeded and hit end state"

    -- Wait for other participants to carry out their interaction steps.
    -- TODO: Timeout based on slot no. Use SM.waitForUpdateUntil
    -- TODO: Investigate: What are expected Block confirmations on Cardano?
    wait = do
      traceLog "activating wait endpoint"
      () <- endpoint @"wait" @()
      traceLog "waiting for update"
      update <- SM.waitForUpdate $ glowClient cfg
      case update of
        -- TODO: Eventually we can match on specific events
        Just (TypedScriptTxOut {tyTxOutData = _state}, _txOutRef) -> do
          traceLog "contract updated, finished waiting"
          move `select` wait
        Nothing -> do
          traceLog "contract ended, finished waiting"
          pure ()

createContract ::
  GlowConfig -> CreateParams -> Contract () GlowSchema GlowError ()
createContract cfg params = do
  initContract cfg params
  traceLog "executing setup move"
  void $ executeMove cfg $ MoveParams (arguments params) "begin0" -- TODO: Derive this from input
  -- also, should this be empty??
  -- Are there no variables we need to supply??
  traceLog "setup move executed"

initContract ::
  GlowConfig -> CreateParams -> Contract () GlowSchema GlowError ()
initContract cfg params = do
  traceLog "create params: " -- <> show params
  let datum = createParamsToGlowDatum params
  traceLog "initialized datum" -- show datum
  void $ awaitSlot 0 -- TODO: grab the timeout from the contract
  let payValue = mkValue 0 -- This is the amount the contract starts with?
  traceLog "initializing contract"
  void $ initContractSM cfg datum payValue
  traceLog "contract initialized"

createParamsToGlowDatum :: CreateParams -> GlowDatum
createParamsToGlowDatum params =
  GlowDatum
    { gdContract = contract params,
      gdVariableMap = arguments params,
      gdFunctionMap = Map.empty,
      gdDatatypeMap = datatypes params,
      gdExecutionPoint = Just "begin0", -- TODO: Get this from params
      gdDeadline = Ledger.Slot (timeoutLength params)
    }

initContractSM :: GlowConfig -> GlowDatum -> Ledger.Value -> Contract () GlowSchema GlowError GlowDatum
initContractSM cfg datum payValue = SM.runInitialise (glowClient cfg) datum payValue

-- TODO: Investigate: How do we verify participant identities?
-- Does validation script include this logic?
executeMove ::
  GlowConfig -> MoveParams -> Contract () GlowSchema GlowError GlowDatum
executeMove cfg params = do
  traceLog "move params" -- show params
  transitionResult <- SM.runStep (glowClient cfg) (moveParamsToRedeemer params)
  case transitionResult of
    SM.TransitionFailure (SM.InvalidTransition s i) -> do
      -- FIXME: proper error handling
      traceLog "Invalid transition"
      error "Invalid Transition" -- TODO: We should Unwind the transaction.
    SM.TransitionSuccess datum -> do
      traceLog "Transition success"
      -- traceLog $ show datum
      return datum

moveParamsToRedeemer :: MoveParams -> GlowRedeemer
moveParamsToRedeemer params = (GlowRedeemer (BSC.pack (entryPoint params), variableMap params))

glowClient :: GlowConfig -> SM.StateMachineClient GlowDatum GlowRedeemer
glowClient cfg =
  SM.mkStateMachineClient $
    SM.StateMachineInstance (machine cfg) (scriptInstance cfg)

traceLog :: String -> Contract () s e ()
traceLog msg =
  void $ trace msg $ logInfo @String msg

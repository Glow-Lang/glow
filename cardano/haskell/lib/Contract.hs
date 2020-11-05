{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Contract where

import Control.Monad.Except
import Control.Monad.State.Strict hiding (state)
import GHC.Stack ()

import Language.PlutusTx.IsData.Class ()
import qualified Language.PlutusCore.Universe as PLC
import Language.PlutusTx.Prelude hiding (lookup, unless)
import qualified Language.PlutusTx.AssocMap as Map
import qualified Language.PlutusTx as PlutusTx
import Ledger.Ada (adaSymbol, adaToken)
import Ledger.Constraints.TxConstraints (TxConstraints (..), mustBeSignedBy, mustPayToPubKey)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger as Ledger
import qualified Ledger.Value as LV
import LedgerBytes
import qualified Language.Plutus.Contract.StateMachine as SM

import Types

data TransitionOutput = TransitionOutput
  { toLabel :: ByteString
  , toConstraints :: TxConstraints SM.Void SM.Void
  , toVariableMap :: VariableMap
  , toFunctionMap :: FunctionMap
  , toDatatypeMap :: DatatypeMap
  , toValue :: LV.Value
  }

transition :: GlowConfig -> SM.State GlowDatum -> GlowRedeemer -> Maybe (TxConstraints SM.Void SM.Void, SM.State GlowDatum)
transition cfg (SM.State (GlowDatum contract variableMap functionMap datatypeMap executionPoint timeout) value') (inputLabel, inputVariables) = do
  guard $ trace "Validating entry point" $ executionPoint == Just inputLabel
  (code, exitPoint) <- trace "Looking up entry point" $ Map.lookup inputLabel contract
  let initialState = TransitionOutput inputLabel mempty variableMap functionMap datatypeMap value'
  case runExcept $ execStateT (mapM evaluateStatement code) initialState of
    Right output -> do
      let newState = GlowDatum contract (toVariableMap output) (toFunctionMap output) (toDatatypeMap output) exitPoint (timeout + gcTimeoutLength cfg)
      pure $ trace "Returning result" (toConstraints output, SM.State newState (toValue output))
    -- TODO: enable use of `Language.PlutusTx.Prelude.traceError` in off-chain code so we can show these
    Left _errMsg ->
      Nothing

  where
    evaluateStatement :: (MonadState TransitionOutput m, MonadError String m) => Statement -> m Value
    evaluateStatement = \case
      Label label ->
        updateLabel label

      Declare varName -> do
        void $ lookupAnyVar (Variable varName)
        return Unit

      Define varName expr -> do
        result <- evaluateExpression expr
        bindVariable varName result

      DefineDatatype name constructors ->
        addDatatype name constructors

      DefineFunction name arg body ->
        addFunction name arg body

      SetParticipant addressRef -> do
        pk <- lookupVar addressRef _PubKey
        addConstraint $ mustBeSignedBy (Ledger.pubKeyHash pk)

      ExpectDeposited amountRef -> do
        amount <- lookupVar amountRef _Integer
        addToValue $ mkValue amount

      ExpectWithdrawn addressRef amountRef -> do
        pk <- lookupVar addressRef _PubKey
        amount <- lookupVar amountRef _Integer
        void $ addConstraint $ mustPayToPubKey (Ledger.pubKeyHash pk) (mkValue amount)
        subtractFromValue $ mkValue amount

      Require booleanRef -> do
        condition <- lookupVar booleanRef _Boolean
        unless condition $
          throwError "Require condition failed."
        return Unit

      Return _ ->
        return Unit

    evaluateExpression :: (MonadState TransitionOutput m, MonadError String m) => Expression -> m Value
    evaluateExpression = \case
      ExpectPublished varName ->
        case Map.lookup varName inputVariables of
          Just publishedValue ->
            return publishedValue
          Nothing ->
            throwError "Expected variable was not published."

      IsValidSignature addressRef digestRef signatureRef -> do
        Ledger.PubKey (LedgerBytes pk) <- lookupVar addressRef _PubKey
        digest <- lookupVar digestRef _ByteString
        Ledger.Signature signature <- lookupVar signatureRef _Signature
        let result = verifySignature pk digest signature
        return (Boolean result)

      Apply functionRef argumentRef -> do
        (argName, body) <- lookupFunction functionRef
        argument <- lookupAnyVar argumentRef
        apply argName body argument


    apply :: (MonadState TransitionOutput m, MonadError String m) => ByteString -> [Statement] -> Value -> m Value
    apply argName body argument = do
      void $ bindVariable argName argument
      results <- mapM evaluateStatement body
      void $ unbindVariable argName
      let (last':_) = reverse results
      return last'

updateLabel :: MonadState TransitionOutput m => ByteString -> m Value
updateLabel label = do
  modify $ \to -> to { toLabel = label }
  return Unit

addConstraint :: MonadState TransitionOutput m => TxConstraints SM.Void SM.Void -> m Value
addConstraint constraint = do
  modify $ \to -> to { toConstraints = toConstraints to <> constraint }
  return Unit

addToValue :: MonadState TransitionOutput m => LV.Value -> m Value
addToValue amount = do
  modify $ \to -> to { toValue = toValue to <> amount }
  return Unit

subtractFromValue :: MonadState TransitionOutput m => LV.Value -> m Value
subtractFromValue amount = do
  modify $ \to -> to { toValue = toValue to <> inv amount }
  return Unit

addFunction :: MonadState TransitionOutput m => ByteString -> ByteString -> [Statement] -> m Value
addFunction name argName body = do
  modify $ \to -> to { toFunctionMap = Map.insert name (argName, body) (toFunctionMap to) }
  return Unit

addDatatype :: MonadState TransitionOutput m => ByteString -> [(ByteString, Integer)] -> m Value
addDatatype name constructors = do
  modify $ \to -> to { toDatatypeMap = Map.insert name constructors (toDatatypeMap to) }
  return Unit

bindVariable :: MonadState TransitionOutput m => ByteString -> Value -> m Value
bindVariable name value' = do
  modify $ \to -> to { toVariableMap = Map.insert name value' (toVariableMap to) }
  return Unit

unbindVariable :: MonadState TransitionOutput m => ByteString -> m Value
unbindVariable name = do
  modify $ \to -> to { toVariableMap = Map.delete name (toVariableMap to) }
  return Unit

lookupVar :: (MonadState TransitionOutput m, MonadError String m) => ValueRef -> (Value -> Maybe a) -> m a
lookupVar valueRef prism = do
  value' <- lookupAnyVar valueRef
  case prism value' of
    Just value'' ->
      return value''
    Nothing ->
      throwError $
           "Type mismatch. "
        <> "Expected: <unknown>." -- TODO: how do we display the expected type without adding a bunch of boilerplate?
        <> "Actual: " <> showType value' <> "."

lookupAnyVar :: (MonadState TransitionOutput m, MonadError String m) => ValueRef -> m Value
lookupAnyVar valueRef = do
  varMap <- gets toVariableMap
  case valueRef of
    Explicit value' ->
      return value'
    Variable variableName ->
      case Map.lookup variableName varMap of
        Just value' -> return value'
        Nothing -> throwError "Missing binding in variable map."


lookupFunction :: (MonadState TransitionOutput m, MonadError String m) => ByteString -> m Function
lookupFunction funcName = do
  funcMap <- gets toFunctionMap
  case Map.lookup funcName funcMap of
    Just func -> return func
    Nothing -> throwError "Missing binding in function map."

showType :: Value -> String
showType = \case
  Constructor _ _ _ -> "Constructor"
  PubKey _ -> "PubKey"
  Signature _ -> "Signature"
  Integer _ -> "Integer"
  ByteString _ -> "ByteString"
  Boolean _ -> "Boolean"
  Unit -> "Unit"

type GlowStateMachine = SM.StateMachine GlowDatum GlowRedeemer

machine :: GlowConfig -> GlowStateMachine
machine cfg = SM.StateMachine
  { SM.smTransition = transition cfg
  , SM.smFinal = isNothing . gdExecutionPoint
  , SM.smCheck = \_datum _redeemer _ctx -> True
      --gdDeadline datum `Ledger.after` Ledger.txInfoValidRange (Ledger.valCtxTxInfo ctx)
  }

mkValue :: Integer -> LV.Value
mkValue = LV.singleton adaSymbol adaToken

mkValidator :: GlowConfig -> Scripts.ValidatorType GlowStateMachine
mkValidator = SM.mkValidator . machine

mkGlowValidatorCode :: GlowConfig -> PlutusTx.CompiledCode PLC.DefaultUni (Scripts.ValidatorType GlowStateMachine)
mkGlowValidatorCode cfg =
  $$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cfg

scriptInstance :: GlowConfig -> Scripts.ScriptInstance GlowStateMachine
scriptInstance cfg = Scripts.validator @GlowStateMachine
  (mkGlowValidatorCode cfg)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GlowDatum @GlowRedeemer
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Glow.Contract where

import Control.Monad.Except hiding (mapM)
import Control.Monad.State.Strict hiding (state)
import Data.String (IsString)
import GHC.Stack ()
import Glow.Types
import qualified Ledger as Ledger
import Ledger.Ada (adaSymbol, adaToken)
import Ledger.Bytes
import Ledger.Constraints.TxConstraints (TxConstraints (..), mustBeSignedBy, mustPayToPubKey)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as LV
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusCore as PLC
import qualified PlutusTx as PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Builtins (String)
import PlutusTx.IsData.Class ()
import PlutusTx.Prelude hiding (lookup, mapM, unless)

data TransitionOutput = TransitionOutput
  { toLabel :: ByteString,
    toConstraints :: TxConstraints SM.Void SM.Void,
    toVariableMap :: VariableMap,
    toFunctionMap :: FunctionMap,
    toDatatypeMap :: DatatypeMap,
    toValue :: LV.Value
  }

-- TODO: Figure out what trace calls are for.
-- Are they meant for on-chain event logging?
-- Or for debugging?
transition :: GlowConfig -> SM.State GlowDatum -> GlowRedeemer -> Maybe (TxConstraints SM.Void SM.Void, SM.State GlowDatum)
transition
  cfg
  ( SM.State
      (GlowDatum contract variableMap functionMap datatypeMap executionPoint timeout)
      value'
    )
  (GlowRedeemer (inputLabel, inputVariables)) = do
    -- TODO: simply make this a product type
    validateEntryPoint executionPoint inputLabel
    (code, exitPoint) <- getCodeAndExitPoint inputLabel contract
    let initialState = TransitionOutput inputLabel mempty variableMap functionMap datatypeMap value'
    case runExcept $ execStateT (mapM (evaluateStatement inputVariables) code) initialState of
      Right output -> do
        let newState = GlowDatum contract (toVariableMap output) (toFunctionMap output) (toDatatypeMap output) exitPoint (timeout + gcTimeoutLength cfg)
        pure $ trace "Returning result" (toConstraints output, SM.State newState (toValue output))
      Left _errMsg ->
        trace _errMsg Nothing

validateEntryPoint :: Maybe ExecutionPoint -> ExecutionPoint -> Maybe ()
validateEntryPoint executionPoint inputLabel =
  guard $
    trace "Validating entry point" $
      executionPoint == Just inputLabel

getCodeAndExitPoint :: ExecutionPoint -> GlowContract -> Maybe ([Statement], Maybe ExecutionPoint)
getCodeAndExitPoint entryPointLabel contract =
  trace "Looking up entry point" $
    Map.lookup entryPointLabel contract

evaluateStatement ::
  forall e m.
  (IsString e, Monoid e, MonadState TransitionOutput m, MonadError e m) =>
  VariableMap ->
  Statement ->
  m GlowValue
evaluateStatement inputVariables = \case
  Label label ->
    updateLabel label
  Declare varName -> do
    void $ lookupAnyVar (Variable varName)
    return Unit
  Define varName expr -> do
    result <- evaluateExpression inputVariables expr
    bindVariable varName result
  DefineDatatype name constructors ->
    addDatatype name constructors
  DefineFunction name arg body ->
    addFunction name arg body
  SetParticipant addressRef -> do
    pk <- lookupVar addressRef _PubKey
    addConstraint $ mustBeSignedBy (Ledger.pubKeyHash pk)

  -- FIXME: This is exactly the same as AddToDeposit?
  ExpectDeposited amountRef -> do
    amount <- lookupVar amountRef _Integer
    addToValue $ mkValue amount
  AddToDeposit amountRef -> do
    amount <- lookupVar amountRef _Integer
    addToValue $ mkValue amount
  ExpectWithdrawn addressRef amountRef -> do
    pk <- lookupVar addressRef _PubKey
    amount <- lookupVar amountRef _Integer
    void $ addConstraint $ mustPayToPubKey (Ledger.pubKeyHash pk) (mkValue amount)
    subtractFromValue $ mkValue amount
  AddToWithdraw addressRef amountRef -> do
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
  Ignore expr -> do
    void $ evaluateExpression inputVariables expr
    return Unit
  DefineInteraction _ _ _ ->
    return Unit
  where
    evaluateExpression ::
      (IsString e, Monoid e, MonadState TransitionOutput m, MonadError e m) =>
      VariableMap ->
      Expression ->
      m GlowValue
    evaluateExpression inputVariables = \case
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
        -- return (Boolean result) -- TODO: Once we generate signatures on client-side, we should see result.
        return (Boolean True) -- FIXME: This is a workaround for clientside, because we currently don't
                              -- properly generate signatures from wallets
      Apply functionRef argumentRef -> do
        (argName, body) <- lookupFunction functionRef
        argument <- lookupAnyVar argumentRef
        apply inputVariables argName body argument
      NoOp ->
        return Unit

    apply ::
      (IsString e, Monoid e, MonadState TransitionOutput m, MonadError e m) =>
      VariableMap ->
      ByteString ->
      [Statement] ->
      GlowValue ->
      m GlowValue
    apply inputVariables argName body argument = do
      void $ bindVariable argName argument
      results <- mapM (evaluateStatement inputVariables) body
      void $ unbindVariable argName
      let (last' : _) = reverse results
      return last'

updateLabel :: MonadState TransitionOutput m => ByteString -> m GlowValue
updateLabel label = do
  modify $ \to -> to {toLabel = label}
  return Unit

addConstraint :: MonadState TransitionOutput m => TxConstraints SM.Void SM.Void -> m GlowValue
addConstraint constraint = do
  modify $ \to -> to {toConstraints = toConstraints to <> constraint}
  return Unit

addToValue :: MonadState TransitionOutput m => LV.Value -> m GlowValue
addToValue amount = do
  modify $ \to -> to {toValue = toValue to <> amount}
  return Unit

subtractFromValue :: MonadState TransitionOutput m => LV.Value -> m GlowValue
subtractFromValue amount = do
  modify $ \to -> to {toValue = toValue to <> inv amount}
  return Unit

addFunction :: MonadState TransitionOutput m => ByteString -> ByteString -> [Statement] -> m GlowValue
addFunction name argName body = do
  modify $ \to -> to {toFunctionMap = Map.insert name (argName, body) (toFunctionMap to)}
  return Unit

addDatatype :: MonadState TransitionOutput m => ByteString -> [(ByteString, Integer)] -> m GlowValue
addDatatype name constructors = do
  modify $ \to -> to {toDatatypeMap = Map.insert name constructors (toDatatypeMap to)}
  return Unit

bindVariable :: MonadState TransitionOutput m => ByteString -> GlowValue -> m GlowValue
bindVariable name value' = do
  modify $ \to -> to {toVariableMap = Map.insert name value' (toVariableMap to)}
  return Unit

unbindVariable :: MonadState TransitionOutput m => ByteString -> m GlowValue
unbindVariable name = do
  modify $ \to -> to {toVariableMap = Map.delete name (toVariableMap to)}
  return Unit

lookupVar :: (IsString e, Monoid e, MonadState TransitionOutput m, MonadError e m) => GlowValueRef -> (GlowValue -> Maybe a) -> m a
lookupVar valueRef prism = do
  value' <- lookupAnyVar valueRef
  case prism value' of
    Just value'' ->
      return value''
    Nothing ->
      throwError $
        "Type mismatch. "
          <> "Expected: <unknown>." -- TODO: how do we display the expected type without adding a bunch of boilerplate?
          <> "Actual: "
          <> showType value'
          <> "."

lookupAnyVar :: (IsString e, Monoid e, MonadState TransitionOutput m, MonadError e m) => GlowValueRef -> m GlowValue
lookupAnyVar valueRef = do
  varMap <- gets toVariableMap
  case valueRef of
    Explicit value' ->
      return value'
    Variable variableName ->
      case Map.lookup variableName varMap of
        Just value' -> return value'
        Nothing -> throwError "Missing binding in variable map."

lookupFunction :: (IsString e, Monoid e, MonadState TransitionOutput m, MonadError e m) => ByteString -> m Function
lookupFunction funcName = do
  funcMap <- gets toFunctionMap
  case Map.lookup funcName funcMap of
    Just func -> return func
    Nothing -> throwError "Missing binding in function map."

showType :: (IsString t, Monoid t) => GlowValue -> t
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
machine cfg =
  SM.StateMachine
    { SM.smTransition = transition cfg,
      SM.smFinal = isNothing . gdExecutionPoint,
      SM.smCheck = \_datum _redeemer _ctx -> True, -- TODO: What exactly is this?
      SM.smThreadToken = Nothing -- TODO: some token to track UTXO??? Just $ gToken game
      -- gdDeadline datum `Ledger.after` Ledger.txInfoValidRange (Ledger.valCtxTxInfo ctx)
    }

mkValue :: Integer -> LV.Value
mkValue = LV.singleton adaSymbol adaToken

-- The inlineable pragma is required for us to compile the validator to plutus core.
-- See: https://plutus-pioneer-program.readthedocs.io/en/latest/week2.html
{-# INLINEABLE mkValidator #-}
mkValidator :: GlowConfig -> Scripts.ValidatorType GlowStateMachine
mkValidator = SM.mkValidator . machine

mkGlowValidatorCode :: GlowConfig -> PlutusTx.CompiledCode (Scripts.ValidatorType GlowStateMachine)
mkGlowValidatorCode cfg =
  $$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode cfg

scriptInstance :: GlowConfig -> Scripts.TypedValidator GlowStateMachine
scriptInstance cfg =
  Scripts.mkTypedValidator @GlowStateMachine
    (mkGlowValidatorCode cfg)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @GlowDatum @GlowRedeemer

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics hiding (Datatype)
import qualified Prelude as P

import qualified Language.PlutusTx as PlutusTx
import Language.PlutusTx.Prelude hiding (lookup, unless)
import Language.PlutusTx.AssocMap (Map)
import qualified Ledger as Ledger
import qualified Ledger.Typed.Scripts as Scripts

data GlowConfig = GlowConfig
  { gcTimeoutLength :: Ledger.Slot
  } deriving (Show)

-- TODO: what's a good default?
-- TODO: convert Glow dates and timestamps to slot numbers
defaultTimeoutLength :: Ledger.Slot
defaultTimeoutLength = Ledger.Slot 100

-- TODO: variable cleanup, only keep live variables between each transaction
type GlowContract = Map ExecutionPoint ([Statement], Maybe ExecutionPoint)
type VariableMap = Map ByteString Value
type FunctionMap = Map ByteString (ByteString, [Statement])
type DatatypeMap = Map ByteString [(ByteString, Integer)]
type Function = (ByteString, [Statement])
type ExecutionPoint = ByteString

data GlowDatum = GlowDatum
  { gdContract :: GlowContract
  , gdVariableMap :: VariableMap
  -- separate from variable map to prevent mutual recursion
  , gdFunctionMap :: FunctionMap
  , gdDatatypeMap :: DatatypeMap
  , gdExecutionPoint :: Maybe ExecutionPoint
  , gdDeadline :: Ledger.Slot
  } deriving (Show)

type GlowRedeemer = (ExecutionPoint, VariableMap)

-- TODO: support lambdas with CPS
data Statement
  = Label ByteString
  | Declare ByteString
  | DefineInteraction [ByteString] [ByteString] [(ByteString, [Statement])]
  | Define ByteString Expression
  | DefineFunction ByteString ByteString [Statement]
  | DefineDatatype ByteString [(ByteString, Integer)]
  | SetParticipant ValueRef
  | ExpectDeposited ValueRef
  | ExpectWithdrawn ValueRef ValueRef
  | AddToDeposit ValueRef
  | AddToWithdraw ValueRef ValueRef
  | Ignore Expression
  | Require ValueRef
  | Return ValueRef
  deriving stock (Generic, P.Eq, Show)
  deriving (FromJSON, ToJSON)

data Expression
  = ExpectPublished ByteString
  | IsValidSignature ValueRef ValueRef ValueRef
  | Apply ByteString ValueRef
  | NoOp
  deriving stock (Generic, P.Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

-- TODO: how to encode expected type?
data ValueRef
  = Explicit Value
  | Variable ByteString
  deriving stock (Generic, P.Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

data Value
  = Constructor ByteString Integer [Value]
  | PubKey Ledger.PubKey
  | Signature Ledger.Signature
  | ByteString ByteString
  | Integer Integer
  | Boolean Bool
  | Unit
  deriving stock (Generic, P.Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

_PubKey :: Value -> Maybe Ledger.PubKey
_PubKey (PubKey pk) = Just pk
_PubKey _ = Nothing

_Signature :: Value -> Maybe Ledger.Signature
_Signature (Signature sig) = Just sig
_Signature _ = Nothing

_ByteString :: Value -> Maybe ByteString
_ByteString (ByteString bs) = Just bs
_ByteString _ = Nothing

_Integer :: Value -> Maybe Integer
_Integer (Integer i) = Just i
_Integer _ = Nothing

_Boolean :: Value -> Maybe Bool
_Boolean (Boolean b) = Just b
_Boolean _ = Nothing

_Unit :: Value -> Maybe ()
_Unit Unit = Just ()
_Unit _ = Nothing

data Glow
instance Scripts.ScriptType Glow where
  type instance RedeemerType Glow = GlowRedeemer
  type instance DatumType Glow = GlowDatum

--PlutusTx.makeIsData ''Datatype
PlutusTx.makeLift ''Value
PlutusTx.makeIsData ''Value
PlutusTx.makeLift ''ValueRef
PlutusTx.makeIsData ''ValueRef
PlutusTx.makeLift ''Expression
PlutusTx.makeIsData ''Expression
PlutusTx.makeLift ''Statement
PlutusTx.makeIsData ''Statement
PlutusTx.makeIsData ''GlowDatum
PlutusTx.makeIsData ''GlowConfig
PlutusTx.makeLift ''GlowConfig

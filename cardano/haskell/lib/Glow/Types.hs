{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Glow.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics hiding (Datatype)
import qualified Ledger as Ledger
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx as PlutusTx
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude hiding (lookup, unless)
import Schema (FormSchema (..), ToArgument, ToSchema (..))
import qualified Prelude as P

data GlowConfig = GlowConfig
  { gcTimeoutLength :: Ledger.Slot
  }
  deriving stock (Generic, P.Show)

-- TODO: what's a good default?
-- TODO: convert Glow dates and timestamps to slot numbers
defaultTimeoutLength :: Ledger.Slot
-- defaultTimeoutLength = Ledger.Slot 100
defaultTimeoutLength = Ledger.Slot 10000

-- TODO: variable cleanup, only keep live variables between each transaction
type GlowContract = Map ExecutionPoint ([Statement], Maybe ExecutionPoint)

type VariableMap = Map ByteString GlowValue

type FunctionMap = Map ByteString (ByteString, [Statement])

type DatatypeMap = Map ByteString [(ByteString, Integer)]

type Function = (ByteString, [Statement])

type ExecutionPoint = ByteString

-- TODO: support lambdas with CPS
data Statement
  = Label ByteString
  | Declare ByteString
  | DefineInteraction [ByteString] [ByteString] [(ByteString, [Statement])]
  | Define ByteString Expression
  | DefineFunction ByteString ByteString [Statement]
  | DefineDatatype ByteString [(ByteString, Integer)]
  | SetParticipant GlowValueRef
  | ExpectDeposited GlowValueRef
  | ExpectWithdrawn GlowValueRef GlowValueRef
  | AddToDeposit GlowValueRef
  | AddToWithdraw GlowValueRef GlowValueRef
  | Ignore Expression
  | Require GlowValueRef
  | Return GlowValueRef
  deriving stock (Generic, P.Eq, P.Show)
  deriving (FromJSON, ToJSON)

instance ToSchema Statement where
  toSchema = FormSchemaString -- TODO: Is this the right way to serialize???

data Expression
  = ExpectPublished ByteString
  | IsValidSignature GlowValueRef GlowValueRef GlowValueRef
  | Apply ByteString GlowValueRef
  | NoOp
  deriving stock (Generic, P.Eq, P.Show)
  deriving anyclass (FromJSON, ToJSON)

-- TODO: how to encode expected type?
data GlowValueRef
  = Explicit GlowValue
  | Variable ByteString
  deriving stock (Generic, P.Eq, P.Show)
  deriving anyclass (FromJSON, ToJSON)

data GlowValue
  = Constructor ByteString Integer [GlowValue]
  | PubKey Ledger.PubKey
  | Signature Ledger.Signature
  | ByteString ByteString
  | Integer Integer
  | Boolean Bool
  | Unit
  deriving stock (Generic, P.Eq, P.Show)
  deriving anyclass (FromJSON, ToJSON) -- ToSchema, ToArgument)

instance ToSchema GlowValue where
  toSchema = FormSchemaString -- TODO: Is this the right way to serialize???

data GlowDatum = GlowDatum
  { gdContract :: GlowContract,
    gdVariableMap :: VariableMap,
    -- separate from variable map to prevent mutual recursion
    gdFunctionMap :: FunctionMap,
    gdDatatypeMap :: DatatypeMap,
    gdExecutionPoint :: Maybe ExecutionPoint,
    gdDeadline :: Ledger.Slot
  }
  deriving stock (Generic, P.Eq, P.Show)

newtype GlowRedeemer = GlowRedeemer (ExecutionPoint, VariableMap) -- TODO flatten this

_PubKey :: GlowValue -> Maybe Ledger.PubKey
_PubKey (PubKey pk) = Just pk
_PubKey _ = Nothing

_Signature :: GlowValue -> Maybe Ledger.Signature
_Signature (Signature sig) = Just sig
_Signature _ = Nothing

_ByteString :: GlowValue -> Maybe ByteString
_ByteString (ByteString bs) = Just bs
_ByteString _ = Nothing

_Integer :: GlowValue -> Maybe Integer
_Integer (Integer i) = Just i
_Integer _ = Nothing

_Boolean :: GlowValue -> Maybe Bool
_Boolean (Boolean b) = Just b
_Boolean _ = Nothing

_Unit :: GlowValue -> Maybe ()
_Unit Unit = Just ()
_Unit _ = Nothing

data Glow

instance Scripts.ValidatorTypes Glow where
  type RedeemerType Glow = GlowRedeemer
  type DatumType Glow = GlowDatum

-- TODO: Use makeIsDataIndexed / other stable representation.
--PlutusTx.makeIsData ''Datatype
PlutusTx.makeLift ''GlowValue
PlutusTx.unstableMakeIsData ''GlowValue

PlutusTx.makeLift ''GlowValueRef
PlutusTx.unstableMakeIsData ''GlowValueRef

PlutusTx.makeLift ''Expression
PlutusTx.unstableMakeIsData ''Expression

PlutusTx.makeLift ''Statement
PlutusTx.unstableMakeIsData ''Statement

PlutusTx.makeLift ''GlowDatum
PlutusTx.unstableMakeIsData ''GlowDatum

PlutusTx.makeLift ''GlowRedeemer
PlutusTx.unstableMakeIsData ''GlowRedeemer

PlutusTx.unstableMakeIsData ''GlowConfig
PlutusTx.makeLift ''GlowConfig

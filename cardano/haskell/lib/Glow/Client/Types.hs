{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Glow.Client.Types where

import Data.Aeson hiding (Value)
import Data.Text (Text)
import GHC.Generics
import Glow.Types
import qualified Ledger as Ledger
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude (ByteString)
import Schema (ToArgument, ToSchema)

type SExprString = String

--------------------------------------------
-- Incoming representation from endpoints --
--------------------------------------------

data RawCreateParams = RawCreateParams
  { source :: SExprString, -- project.sexp
    initialVariableMap :: SExprString,   -- initial arguments to initialize contract interaction
    rawTimeoutLength :: Integer
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data RawMoveParams = RawMoveParams
  { rawVariableMap :: SExprString,
    rawEntryPoint :: String
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

------------------------------------------
-- Representation within smart contract --
------------------------------------------
-- NOTE: Eventually parsing logic should be glow's responsibility
-- and this should be expected shape of data at endpoints.

data CreateParams = CreateParams
  { datatypes :: DatatypeMap,
    participants :: Map ByteString Ledger.PubKey, -- TODO: type synonym for this
    arguments :: VariableMap,
    contract :: GlowContract, -- consensus program
    timeoutLength :: Integer
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MoveParams = MoveParams
  { variableMap :: VariableMap,
    entryPoint :: String
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

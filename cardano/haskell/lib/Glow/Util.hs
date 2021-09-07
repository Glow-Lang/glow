{- ORMOLU_DISABLE -}
{-# LANGUAGE LambdaCase #-}

module Util where

import Data.ByteUnits

import qualified PlutusTx as PlutusTx
import PlutusCore
import Ledger hiding (value, strip)
import qualified Ledger.Typed.Scripts as Scripts

printSizeStats
  :: PlutusTx.CompiledCode DefaultUni a -> Scripts.ScriptInstance x -> IO ()
printSizeStats unwrappedScript wrappedScript = do
  putStrLn $ "unwrapped untyped: " <> asByteSize (untypedUnwrapped unwrappedScript)
  putStrLn $ "unwrapped typed:   " <> asByteSize (typedUnwrapped unwrappedScript)
  putStrLn $ "wrapped untyped:   " <> asByteSize (untypedWrapped wrappedScript)
  putStrLn $ "wrapped typed:     " <> asByteSize (typedWrapped wrappedScript)
  where
    asByteSize size =
      getShortHand $ getAppropriateUnits (ByteValue (fromInteger size) Bytes)

untypedWrapped :: Scripts.ScriptInstance x -> Integer
untypedWrapped script =
  scriptSize $ unValidatorScript $ Scripts.validatorScript script

typedWrapped :: Scripts.ScriptInstance x -> Integer
typedWrapped script =
  scriptSize $ unValidatorScript $ Scripts.validatorScript script

untypedUnwrapped :: PlutusTx.CompiledCode DefaultUni a -> Integer
untypedUnwrapped script =
  scriptSize $ fromCompiledCode script

typedUnwrapped :: PlutusTx.CompiledCode DefaultUni a -> Integer
typedUnwrapped script =
  scriptSize $ fromCompiledCode script

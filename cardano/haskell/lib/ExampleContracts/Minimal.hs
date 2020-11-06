{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}

module ExampleContracts.Minimal where

import Ledger hiding (value)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Scripts (ScriptType)
import Ledger.Typed.Scripts.Validators (RedeemerType, DatumType)
import qualified Language.PlutusTx as PlutusTx
import Language.PlutusCore.Universe

data Minimal
instance ScriptType Minimal where
  type instance RedeemerType Minimal = ()
  type instance DatumType Minimal = ()

validate :: () -> () -> ValidatorCtx -> Bool
validate _ _ _ = True

unwrappedScript :: PlutusTx.CompiledCode DefaultUni (() -> () -> ValidatorCtx -> Bool)
unwrappedScript =
  $$(PlutusTx.compile [|| validate ||])

wrappedScript :: Scripts.ScriptInstance Minimal
wrappedScript = Scripts.validator @Minimal
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @() @()
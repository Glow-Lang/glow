-- | NOTE: Currently we use `GlowMain` instead
-- to run the contract, because it allows us
-- to simulate the on-chain part of things as well.
-- Eventually when we have a way to deploy the on-chain part
-- Ideally we can interact with it via this.
module ContractMain where

import Data.Bifunctor (first)
import Data.Text.Extras (tshow)
import Plutus.PAB.ContractCLI (commandLineApp)
import Glow.Client
import Glow.Types

main :: IO ()
main = do
  let cfg = GlowConfig defaultTimeoutLength
  commandLineApp $ first tshow $ glowContract cfg

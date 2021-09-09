-- | NOTE: Currently we use `GlowMain` instead
-- to run the contract via the simulator.
-- ContractMain (this app) will be have to be installed and run via the PAB-cli when we want to actually deploy on-chain.
-- For installation instructions,
-- See: https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc
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

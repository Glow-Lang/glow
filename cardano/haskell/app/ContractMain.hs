
import Data.Bifunctor (first)
import Plutus.SCB.ContractCLI (commandLineApp)
import Plutus.SCB.Utils (tshow)

import Client
import Types

main :: IO ()
main = do
  let cfg = GlowConfig defaultTimeoutLength
  commandLineApp $ first tshow $ glowContract cfg
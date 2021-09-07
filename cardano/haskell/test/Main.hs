{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Test.Glow.Contract (contractTests)
import Test.Glow.Misc (miscTests)
import Test.Glow.Parser (parseTests)
import Test.Integration.Glow.Contract (contractIntegrationTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "glow unit-tests"
      [ contractTests,
        miscTests,
        parseTests,
        contractIntegrationTests
      ]

module Test.Glow.Parser where

import Glow.Parser (parseRawCreateParams, parseRawMoveParams)
import qualified Test.Glow.Samples as Samples
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

parseTests :: TestTree
parseTests =
  testGroup
    "parse"
    [ parseCreateParamsTest,
      parseMoveParams0Test,
      parseMoveParams1Test
    ]

parseCreateParamsTest :: TestTree
parseCreateParamsTest = testCase "parse /create" $ do
  assertEqual "Should parse correct input" expected actual
  where
    expected = Samples.buySigParsedCreateParams
    actual = parseRawCreateParams Samples.buySigRawCreateParams

parseMoveParams0Test :: TestTree
parseMoveParams0Test = testCase "parse /move step-1" $ do
  assertEqual "Should parse correct input" expected actual
  where
    expected = Samples.buySigMoveParams0
    actual = parseRawMoveParams Samples.buySigRawMoveParams0

parseMoveParams1Test :: TestTree
parseMoveParams1Test = testCase "parse /move step-2" $ do
  assertEqual "Should parse correct input" expected actual
  where
    expected = Samples.buySigMoveParams1
    actual = parseRawMoveParams Samples.buySigRawMoveParams1

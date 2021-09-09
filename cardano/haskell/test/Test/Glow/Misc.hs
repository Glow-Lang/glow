{-# LANGUAGE OverloadedStrings #-}

module Test.Glow.Misc where

import qualified Data.ByteString as BS
import Ledger.Bytes (LedgerBytes (..))
import Ledger.Crypto (PubKey (..), Signature (..))
import Plutus.Contract.Test (walletPubKey)
import PlutusTx.Builtins (verifySignature)
import Test.Glow.Samples (seller)
import Test.Glow.Utils (generateSignature)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

miscTests :: TestTree
miscTests = testGroup "misc" [verifySignatureTest]

-- TODO: Should we even be testing this?? This is functionality from external library.
-- Ported over from older version of glow-cardano.
verifySignatureTest :: TestTree
verifySignatureTest = testCase "signing" $ do
  let Signature signature = generateSignature message seller
      message = "test" :: BS.ByteString
      Ledger.Crypto.PubKey (LedgerBytes pk) = walletPubKey seller
  assertBool "verify signature" (verifySignature pk message signature)

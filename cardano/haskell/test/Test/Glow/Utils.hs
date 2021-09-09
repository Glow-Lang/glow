{-# LANGUAGE OverloadedStrings #-}

module Test.Glow.Utils where

import qualified Crypto.ECC.Ed25519Donna as ED25519
import Crypto.Error (CryptoFailable (..))
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.Word8 (Word8)
import Glow.Client.Types (SExprString)
import Glow.Types (GlowDatum (..))
import qualified Ledger.Bytes as KB
import Ledger.Crypto (Signature (..), getPrivateKey)
import qualified Plutus.Contract.StateMachine as SM
import Plutus.Contract.Test (Wallet (..), walletPrivKey)
import Test.Tasty.HUnit (assertEqual, assertFailure)

-- TODO: Maybe this should be moved into Contract module??
generateSignature :: BS.ByteString -> Wallet -> Signature
generateSignature msg wallet =
  let sk = ED25519.secretKey $ KB.getLedgerBytes (getPrivateKey $ walletPrivKey wallet)
      pk = ED25519.toPublic <$> sk
      salt :: BS.ByteString
      salt = ""
   in case ED25519.sign <$> sk <*> pure salt <*> pk <*> pure msg of
        CryptoPassed signature ->
          Signature $ BS.pack $ BA.unpack signature
        CryptoFailed err ->
          error $ show err

-- TODO: Rename to association list
makePairList :: [(String, String)] -> SExprString
makePairList pairs =
  "(" <> concatMap mkPair pairs <> ")"
  where
    mkPair (k, v) = "(" <> k <> " . " <> v <> ")"

-- TODO: Maybe this should be moved into Utils module?
hexStringToBS :: String -> ByteString
hexStringToBS = BS.pack . hexStringToWord8s

hexStringToWord8s :: String -> [Word8]
hexStringToWord8s [] = []
hexStringToWord8s [c] = [hexSingleToWord c]
hexStringToWord8s (c1 : c2 : s) = hexDoubleToWord c1 c2 : hexStringToWord8s s

hexDoubleToWord :: Char -> Char -> Word8
hexDoubleToWord h1 h2 =
  hexSingleToWord h1 `shiftL` 4
    .|. hexSingleToWord h2

hexSingleToWord :: Char -> Word8
hexSingleToWord = fromIntegral . digitToInt

assertTransitStateEq ::
  Maybe (a, SM.State GlowDatum) -> -- TODO should we be using 'a' to provide additional info
  -- during failure cases?
  SM.State GlowDatum ->
  IO ()
assertTransitStateEq Nothing _ = assertFailure "Transition failed"
assertTransitStateEq (Just (_, sActual)) sExpected = assertEqual "Should Transit" sExpected sActual

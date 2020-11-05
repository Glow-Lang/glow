{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

--import Control.Monad.Freer
import qualified Crypto.ECC.Ed25519Donna as ED25519
import Crypto.Error
import Data.Aeson.Extras as JSON
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ

import Language.PlutusTx.Builtins hiding (String, error)
import Language.Plutus.Contract.Test
import Language.PlutusTx.Lattice
import Ledger.Crypto (PubKey(..), Signature (..), getPrivateKey, getPubKey)
import LedgerBytes (LedgerBytes(..))
import qualified LedgerBytes as KB

import Client
import Parser
import Types hiding (Signature)

main :: IO ()
main =
  defaultMain $
     testGroup "glow"
       [ testCase "signing" verifySignatureTest
       , contractTests
       ]

buyer, seller :: Wallet
buyer = Wallet 1
seller = Wallet 2

buySigHeader :: SExprString
buySigHeader =
  [r|(@header
      (Buyer Seller)
      ((digest0 : Digest) (price : nat)))|]

buySigBody :: SExprString
buySigBody =
  [r|(@body
      (@label begin0)
      (@label cp)
      (consensus:set-participant Buyer)
      (expect-deposited price)
      (@label cp0)
      (consensus:set-participant Seller)
      (def signature (expect-published 'signature))
      (def tmp (@app isValidSignature Seller digest0 signature))
      (require! tmp)
      (expect-withdrawn Seller price)
      (return (@tuple))
      (@label end0))|]

digestVal :: BS.ByteString
digestVal = "digest"

initialBuySigVarMap :: SExprString
initialBuySigVarMap = makePairList
  [ ("Buyer", "(pub-key " <> show (show (getPubKey $ walletPubKey buyer)) <> ")")
  , ("Seller", "(pub-key " <> show (show (getPubKey $ walletPubKey seller)) <> ")")
  , ("digest0", show digestVal)
  , ("price", "100")
  ]

sellerBuySigVarMap :: SExprString
sellerBuySigVarMap = makePairList
  [ ("signature", "(signature " <> show (JSON.encodeByteString signature) <> ")")
  ]
  where
    Signature signature = generateSignature digestVal seller

emptyVarMap :: SExprString
emptyVarMap = makePairList []

-- TODO: Rename to association list
makePairList :: [(String, String)] -> SExprString
makePairList pairs =
  "(" <> concatMap mkPair pairs <> ")"
  where
    mkPair (k, v) = "(" <> k <> " . " <> v <> ")"

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

verifySignatureTest :: IO ()
verifySignatureTest = do
  let Signature signature = generateSignature message seller
      message = "test" :: BS.ByteString
      Ledger.Crypto.PubKey (LedgerBytes pk) = walletPubKey seller
  assertBool "verify signature" (verifySignature pk message signature)

contractTests :: TestTree
contractTests = testGroup "contract"
  [ checkPredicate @GlowSchema @GlowError
      "successful buy signature run" (glowContract $ GlowConfig defaultTimeoutLength)
          (assertDone buyer (const True) "contract should close" /\
           assertDone seller (const True) "contract should close")
            -- add to force contract logs:
            -- emulatorLog (const False) ""
        emulateBuySignature
  ]

emulateBuySignature :: ContractTrace GlowSchema GlowError () ()
emulateBuySignature = do

  callEndpoint @"wait" buyer ()
  handleBlockchainEvents buyer

  callEndpoint @"create" seller (CreateParams buySigHeader buySigBody initialBuySigVarMap 100)
  handleBlockchainEvents seller
  addBlocks 1
  handleBlockchainEvents seller
  addBlocks 1
  handleBlockchainEvents seller

  handleBlockchainEvents buyer
  callEndpoint @"move" buyer (MoveParams emptyVarMap "cp")
  handleBlockchainEvents buyer
  addBlocks 1
  handleBlockchainEvents buyer

  handleBlockchainEvents seller
  handleBlockchainEvents seller
  callEndpoint @"move" seller (MoveParams sellerBuySigVarMap "cp0")
  handleBlockchainEvents seller
  addBlocks 1
  handleBlockchainEvents seller
  handleBlockchainEvents buyer



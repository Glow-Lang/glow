{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Glow.Samples where

import qualified Data.Aeson.Extras as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Glow.Client (createParamsToGlowDatum, moveParamsToRedeemer)
import Glow.Client.Types (CreateParams (..), MoveParams (..), RawCreateParams (..), RawMoveParams (..), SExprString)
import Glow.Contract (mkValue)
import Glow.Types
  ( ExecutionPoint,
    Expression (..),
    GlowConfig (..),
    GlowContract,
    GlowDatum (..),
    GlowRedeemer (..),
    GlowValueRef (..),
    Statement (..),
    VariableMap,
    defaultTimeoutLength,
  )
import qualified Glow.Types as Glow -- necessary for some Glow types / constructors which shadow Plutus names
import Ledger.Bytes (LedgerBytes (..))
import Ledger.Crypto (PubKey (..), Signature (..), getPubKey)
import Ledger.Slot (Slot (..))
import Ledger.Value (Value (..))
import qualified Plutus.Contract.StateMachine as SM
import Plutus.Contract.Test (Wallet (..), walletPubKey)
import qualified PlutusTx.AssocMap as Map
import Test.Glow.Utils (generateSignature, hexStringToBS, makePairList)
import Text.RawString.QQ

-- TODO: Use Closing namespace for all buysig samples

------------ Wallets

buyer :: Wallet
buyer = Wallet 1

buyerPubKey :: PubKey
buyerPubKey = PubKey $ LedgerBytes buyerPubKeyBS

buyerPubKeyBS :: ByteString
buyerPubKeyBS = hexStringToBS "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"

sellerPubKey :: PubKey
sellerPubKey = PubKey $ LedgerBytes sellerPubKeyBS

sellerPubKeyBS :: ByteString
sellerPubKeyBS = hexStringToBS "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"

seller :: Wallet
seller = Wallet 2

------------ Config

glowConfig :: GlowConfig
glowConfig = GlowConfig defaultTimeoutLength

------------ Closing Contract Parameters

digestVal :: BS.ByteString
digestVal = "digest"

buySigParsedCreateParams :: CreateParams
buySigParsedCreateParams =
  CreateParams
    { datatypes = Map.fromList [],
      participants =
        Map.fromList -- TODO: This seems to be overlap with arguments.
          [ ("Buyer", buyerPubKey),
            ("Seller", sellerPubKey)
          ],
      arguments =
        Map.fromList -- FIXME: This should be initial MoveParams right???
          [ ("Buyer", Glow.PubKey buyerPubKey),
            ("Seller", Glow.PubKey sellerPubKey),
            ("digest0", Glow.ByteString "digest"),
            ("price", Glow.Integer 100)
          ],
      contract =
        Map.fromList
          [ ( "begin0",
              ( [SetParticipant (Variable "Buyer"), ExpectDeposited (Variable "price")],
                Just "cp0"
              )
            ),
            ( "cp0",
              ( [ SetParticipant (Variable "Seller"),
                  Define "signature" (ExpectPublished "signature"),
                  Define
                    "tmp"
                    ( IsValidSignature
                        (Variable "Seller")
                        (Variable "digest0")
                        (Variable "signature")
                    ),
                  Require (Variable "tmp"),
                  AddToWithdraw
                    (Variable "Seller")
                    (Variable "price"),
                  Return (Variable "@tuple"),
                  Label "end0"
                ],
                Nothing
              )
            )
          ],
      timeoutLength = 100
    }

buySigInitialState :: SM.State GlowDatum
buySigInitialState = SM.State buySigInitialDatum buySigInitialValue

buySigInitialDatum :: GlowDatum
buySigInitialDatum = createParamsToGlowDatum buySigParsedCreateParams

buySigInitialValue :: Value
buySigInitialValue = mkValue 100

buySigInitialMoveParams :: MoveParams
buySigInitialMoveParams = MoveParams (arguments buySigParsedCreateParams) "begin0" -- TODO This should be abstracted

buySigInitialRedeemer :: GlowRedeemer
buySigInitialRedeemer = moveParamsToRedeemer buySigInitialMoveParams

buySigRawCreateParams :: RawCreateParams
buySigRawCreateParams = RawCreateParams contractSource initialBuySigVarMap 100

emptyVarMap :: SExprString
emptyVarMap = makePairList []

initialBuySigVarMap :: SExprString
initialBuySigVarMap =
  makePairList
    [ ("Buyer", "(pub-key " <> show (show (getPubKey $ walletPubKey buyer)) <> ")"),
      ("Seller", "(pub-key " <> show (show (getPubKey $ walletPubKey seller)) <> ")"),
      ("digest0", show digestVal),
      ("price", "100")
    ]

-- TODO use this for parser tests
sellerBuySigVarMap :: SExprString
sellerBuySigVarMap =
  makePairList
    [ ("signature", "(signature " <> show (JSON.encodeByteString signature) <> ")")
    ]
  where
    Signature signature = generateSignature digestVal seller

-- project.sexp output from Glow
contractSource :: SExprString
contractSource =
  [r|(@module (begin end)
         (@label begin)
         (def buySig
              (@make-interaction
               ((@list Buyer Seller))
               (digest0 price)
               (begin0 end0)
               (#f
                (@label begin0)
                (@label cp)
                (consensus:set-participant Buyer)
                (expect-deposited price)
                (@label cp0)
                (consensus:set-participant Seller)
                (consensus:set-participant Seller)
                (def signature (expect-published 'signature))
                (def tmp
                     (@app isValidSignature Seller digest0 signature))
                (require! tmp)
                (consensus:withdraw Seller price)
                (return (@tuple))
                (@label end0))
               (Buyer (@label begin0) ;; safe point
                      (@label cp) ;; redundant safe point, seller waits but buyer doesn't
                      (participant:set-participant Buyer)
                      (add-to-deposit price) ;; modifies buffer or fails
                      (@label cp0) ;; safe point, buyers waits and seller starts
                      (participant:set-participant Seller)
                      ;; flush buffer, contract initialization (no contract receipt), check cpitable2 for live public variables at checkpoint
                      (participant:set-participant Seller)
                      (def signature (expect-published 'signature))
                      (def tmp
                           (@app isValidSignature
                                 Seller
                                 digest0
                                 signature))
                      (require! tmp)
                      (participant:withdraw Seller price)
                      (return (@tuple))
                      (@label end0))
               (Seller (@label begin0)
                       (@label cp)
                       (participant:set-participant Buyer)
                       (expect-deposited price)
                       (@label cp0)
                       (participant:set-participant Seller)
                       (def signature (sign digest0))
                       (participant:set-participant Seller)
                       (add-to-publish 'signature signature)
                       (def tmp
                            (@app isValidSignature
                                  Seller
                                  digest0
                                  signature))
                       (require! tmp)
                       (participant:withdraw Seller price)
                       (return (@tuple))
                       (@label end0))))
         (return (@tuple))
         (@label end))|]

------------ Closing Move Params

buySigMoveParams0 :: MoveParams
buySigMoveParams0 = MoveParams Map.empty "cp"

buySigRawMoveParams0 :: RawMoveParams
buySigRawMoveParams0 = RawMoveParams emptyVarMap "cp"

buySigMoveParams1 :: MoveParams
buySigMoveParams1 = MoveParams varMap "cp0"
  where
    varMap = Map.fromList [("signature", signature)]
    signature =
      Glow.Signature $
        Signature $
          hexStringToBS
            "a42c998763bee0e8\
            \091e36515bc318a4\
            \6baeb46cf15259a9\
            \bfc9e1365a499ac6\
            \b57517239d693995\
            \07198680e8987406\
            \46bd8c368f91e1c6\
            \58baae560038410a"

buySigRawMoveParams1 :: RawMoveParams
buySigRawMoveParams1 = RawMoveParams sellerBuySigVarMap "cp0"

------------ Transition values

initialContractEntryPoint :: Maybe ExecutionPoint
initialContractEntryPoint = gdExecutionPoint buySigInitialDatum

initialParamsEntryPoint :: ExecutionPoint
initialParamsEntryPoint = case buySigInitialRedeemer of
  GlowRedeemer (inputLabel, _) -> inputLabel

buySigGlowContract :: GlowContract
buySigGlowContract = gdContract buySigInitialDatum

buySigInitialCode :: [Statement]
buySigInitialCode =
  [ SetParticipant (Variable "Buyer"),
    ExpectDeposited (Variable "price")
  ]

buySigInitialVarMap :: VariableMap
buySigInitialVarMap = gdVariableMap buySigInitialDatum

-- TODO:
-- buySigInitialSMState
-- derived from datum and redeemer:
-- data TransitionOutput = TransitionOutput
--   { toLabel :: ByteString
--   , toConstraints :: TxConstraints SM.Void SM.Void
--   , toVariableMap :: VariableMap
--   , toFunctionMap :: FunctionMap
--   , toDatatypeMap :: DatatypeMap
--   , toValue :: LV.Value
--   }

buySigInitialPrice :: Integer
buySigInitialPrice = 100

buySigInitialExitPoint :: Maybe ExecutionPoint
buySigInitialExitPoint = Just "cp0"

buySigTransitionState1 :: SM.State GlowDatum
buySigTransitionState1 =
  SM.State
    { SM.stateData = buySigTransitionDatum1,
      SM.stateValue = buySigTransitionValue1
    }

buySigTransitionDatum1 :: GlowDatum
buySigTransitionDatum1 =
  GlowDatum
    { gdContract =
        Map.fromList
          [ ( "begin0",
              ( [ SetParticipant (Variable "Buyer"),
                  ExpectDeposited (Variable "price")
                ],
                Just "cp0"
              )
            ),
            ( "cp0",
              ( [ SetParticipant (Variable "Seller"),
                  Define "signature" (ExpectPublished "signature"),
                  Define
                    "tmp"
                    ( IsValidSignature
                        (Variable "Seller")
                        (Variable "digest0")
                        (Variable "signature")
                    ),
                  Require (Variable "tmp"),
                  AddToWithdraw (Variable "Seller") (Variable "price"),
                  Return (Variable "@tuple"),
                  Label "end0"
                ],
                Nothing
              )
            )
          ],
      gdVariableMap =
        Map.fromList
          [ ("Buyer", Glow.PubKey buyerPubKey),
            ("Seller", Glow.PubKey sellerPubKey),
            ("digest0", Glow.ByteString "digest"),
            ("price", Glow.Integer 100)
          ],
      gdFunctionMap = Map.fromList [],
      gdDatatypeMap = Map.fromList [],
      gdExecutionPoint = Just "cp0",
      gdDeadline = Slot {getSlot = 10100}
    }

buySigTransitionValue1 :: Value
buySigTransitionValue1 = mkValue 200

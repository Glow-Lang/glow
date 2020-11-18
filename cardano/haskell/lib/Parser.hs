{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Parser where

import Control.Lens (over, _1)
import Control.Monad.State
import Data.Aeson.Extras (tryDecode)
import Data.Function
import Data.String (fromString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Void
import System.Environment
import Text.SExpression as SExpr
import Text.Megaparsec hiding (Label, State)
--import Debug.Trace

import Language.PlutusTx.Prelude (ByteString)
import Language.PlutusTx.AssocMap (Map)
import qualified Language.PlutusTx.AssocMap as Map
import qualified Ledger.Crypto as Ledger

import Types

type SExprString = String

pattern Builtin :: String -> [SExpr] -> SExpr
pattern Builtin head tail = List (Atom head:tail)

pattern Pair :: String -> String -> SExpr
pattern Pair fst snd = List [Atom fst, Atom snd]

parseCommand :: IO ()
parseCommand = do
  filePath:_ <- getArgs
  contractSource <- readFile filePath
  putStrLn $ case parse (parseSExpr def) filePath contractSource of
    Right contractSexpr ->
      let program = extractPrograms (parseModule contractSexpr)
      in show $
        "Participants:" <+> prettyList (show <$> _participants program) <> line <>
        "Arguments:" <+> prettyList (show <$> _arguments program) <> line <> line <>
        "Consensus program:" <+> line <> prettyContract (_consensusProgram program) <> line <>
        vsep (fmap (\(participant, contract) -> pretty (BSC.unpack participant) <> " program:" <+> line <> prettyContract contract) (Map.toList $ _participantPrograms program))
    Left err ->
      errorBundlePretty err


prettyContract :: GlowContract -> Doc ann
prettyContract glowContract =
  indent 2 $
    vsep $ (\(k, (stmts, maybeExit)) ->
      "->" <+> viaShow k <> line <>
      indent 4 (vsep (viaShow <$> stmts)) <> line <>
      maybe "" (\exit -> "<-" <+> viaShow exit) maybeExit
      ) <$> Map.toList glowContract


parseModule :: SExpr -> [Statement]
parseModule = \case
  List (Atom "@module":Pair _startLabel _endLabel:statements) ->
    parseStatement <$> statements
  unknown ->
    error $ "Invalid module format: " <> show unknown

parseStatement :: SExpr -> Statement
parseStatement = \case
  Builtin "@label" [Atom name] ->
    Label $ BSC.pack name

  Builtin "deftype" [Atom _name, _typeDefinition] ->
    error "monomorphic type not supported"

  Builtin "deftype" [List (Atom _name:_typeVariables), _typeDefinition] ->
    error "polymorphic type not supported"

  Builtin "defdata" [Atom _name, _datatypeDefinition] ->
    error "monomorphic datatype not supported"

  Builtin "defdata" [List (Atom _name:_typeVariables), _datatypeDefinition] ->
    error "polymorphic datatype not supported"

  Builtin "def"
    [ Atom _contractName
    , Builtin "@make-interaction"
      ( List [ Builtin "@list" participantNames]
      : List argumentNames
      : Pair _startLabel _endLabel
      : interactions
      )
    ] ->
    DefineInteraction
      (BSC.pack . parseName <$> participantNames)
      (BSC.pack . parseName <$> argumentNames)
      (parseInteraction <$> interactions)

  Builtin "def" [Atom variableName, Builtin "λ" (Atom argName:body)] ->
    DefineFunction (BSC.pack variableName) (BSC.pack argName) (parseStatement <$> body)

  Builtin "def" [Atom variableName, sexpr] ->
    Define (BSC.pack variableName) (parseExpression sexpr)

  Builtin "ignore!" [sexpr] ->
    Ignore (parseExpression sexpr)

  Builtin "return" [List [Atom typeName]] ->
    Return $ var typeName

  Builtin "consensus:set-participant" [roleName] ->
    SetParticipant (var $ parseName roleName)

  Builtin "participant:set-participant" [roleName] ->
    SetParticipant (var $ parseName roleName)

  Builtin "expect-deposited" [Atom amountName] ->
    ExpectDeposited (var amountName)

  Builtin "expect-withdrawn" [Atom roleName, Atom amountName] ->
    ExpectWithdrawn (var roleName) (var amountName)

  Builtin "add-to-publish" _ ->
    Require $ Explicit (Boolean True)

  Builtin "add-to-deposit" [Atom amountName] ->
    AddToDeposit (var amountName)

  Builtin "add-to-withdraw" [Atom roleName, Atom amountName] ->
    AddToWithdraw (var roleName) (var amountName)

  Builtin "require!" [Atom variableName] ->
    Require $ var variableName

  Builtin "assert!" [Atom variableName] ->
    Require $ var variableName

  Builtin "switch" (Atom _argumentExpression:_patterns) ->
    error "switch statements are not supported"

  unknown ->
    error $ "Unknown statement in contract body: " <> show unknown

parseExpression :: SExpr -> Expression
parseExpression = \case
  Builtin "expect-published" [variableName] ->
    ExpectPublished (BSC.pack $ parseName variableName)

  Builtin "@app" [Atom "isValidSignature", Atom roleName, Atom digestVariableName, Atom signatureVariableName] ->
    IsValidSignature (var roleName) (var digestVariableName) (var signatureVariableName)

  Builtin "sign" [Atom _variableName] ->
    NoOp

  unknown ->
    error $ "Unknown expression in contract body: " <> show unknown

parseInteraction :: SExpr -> (ByteString, [Statement])
parseInteraction = \case
  Builtin participantName statements ->
    (BSC.pack participantName, parseStatement <$> statements)

  List (Bool False:statements) ->
    (BSC.pack "consensus", parseStatement <$> statements)

  unknown ->
    error $ "Invalid participant interaction expression: " <> show unknown

parseContractHeader :: SExpr -> [Statement]
parseContractHeader = \case
  List (Atom "@header":content) ->
    join $ run <$> content
  unknown ->
    error $ "Unknown header format: " <> show unknown

  where
    run = \case
      List (Atom "defdata":Atom name:constructorSExps) ->
        let constructors = over _1 BSC.pack . parseConstructor <$> constructorSExps
        in [DefineDatatype (BSC.pack name) constructors]

      List (List arg:args) ->
        flip fmap (List arg:args) $ \case
          List [Atom name, _, _] -> Declare $ BSC.pack name
          unknown -> error $ "Invalid argument value: " <> show unknown

      List participants ->
        flip fmap participants $ \case
          Atom name -> Declare $ BSC.pack name
          unknown -> error $ "Invalid participant value: " <> show unknown

      unknown ->
        error $ "Unknown expression in contract header: " <> show unknown

    parseConstructor = \case
      Atom name ->
        (name, 0)
      List (Atom name:args) ->
        (name, toInteger (length args))
      unknown ->
        error $ "Invalid constructor expression: " <> show unknown


parseContractBody :: SExpr -> [Statement]
parseContractBody = \case
  List (Atom "@body":content) ->
    parseStatement <$> content

  unknown ->
    error $ "Unknown body format: " <> show unknown

parseInputs :: DatatypeMap -> SExprString -> Either (ParseErrorBundle String Void) (Map ByteString Value)
parseInputs datatypes variableMapStr = do
  variableMapSExpr <- parse (parseSExpr def) "body" variableMapStr
  pure $ parseVariableMap datatypes variableMapSExpr

parseVariableMap :: DatatypeMap -> SExpr -> Map ByteString Value
parseVariableMap _datatypes = \case
  List pairs ->
    Map.fromList $ parsePair <$> pairs
  unknown ->
    error $ "Invalid map expression: " <> show unknown
  where
    parsePair = \case
      ConsList [Atom varName] varValue ->
        let value = case varValue of
              Number number ->
                Integer number

              SExpr.String string ->
                Types.ByteString (BSC.pack string)

              Bool bool ->
                Boolean bool

              List [Atom cons, SExpr.String val] ->
                parseDatatype cons val

              unknown ->
                error $ "Invalid variable value: " <> show unknown
        in (BSC.pack varName, value)

      List [Atom varName, Atom cons, SExpr.String val] ->
        (BSC.pack varName, parseDatatype cons val)

      unknown ->
        error $ "Invalid pair expression: " <> show unknown

    parseDatatype "signature" rawSignature =
      case tryDecode (Text.pack rawSignature) of
        Right signature ->
          Signature (Ledger.Signature signature)
        Left err ->
          error $ "Invalid signature value: " <> err

    parseDatatype "pub-key" rawPubKey =
      PubKey (fromString rawPubKey)

    parseDatatype cons _rawVal =
      error $ "Unknown constructor: " <> cons


data GlowProgram = GlowProgram
  { _participants :: [ByteString]
  , _arguments :: [ByteString]
  , _consensusProgram :: GlowContract
  , _participantPrograms :: Map ByteString GlowContract
  }

extractPrograms :: [Statement] -> GlowProgram
extractPrograms statements =
  execState (traverse processHeaderStatement statements) initialState
  where
    initialState = GlowProgram
      { _participants = []
      , _arguments = []
      , _consensusProgram = Map.empty
      , _participantPrograms = Map.empty
      }

    processHeaderStatement = \case
      DefineInteraction participants arguments interactions -> do
        modify $ \program -> program { _participants = participants, _arguments = arguments }
        let consensusProgram = processProgram "consensus" interactions
        let participantPrograms = (\participant -> processProgram participant interactions) <$> participants
        modify $ \program -> program
          { _consensusProgram = snd consensusProgram
          , _participantPrograms = Map.fromList participantPrograms
          }
      _ ->
        pure ()

    processProgram name interactions =
      case lookup name interactions of
        Just consensusStatements ->
          let (_, _, result) = execState (traverse processBodyStatement consensusStatements) (Nothing, "begin0", Map.empty)
          in (name, result)
        Nothing ->
          error $ "Contract is missing " <> BSC.unpack name <> " code."

    processBodyStatement = \case
      SetParticipant newParticipant ->
        setParticipant newParticipant
      stmt ->
        addStatement stmt

    setParticipant :: ValueRef -> State (Maybe ValueRef, ExecutionPoint, Map ExecutionPoint ([Statement], Maybe ExecutionPoint)) ()
    setParticipant newParticipant =
      modify $ \cur@(curParticipant, curLabel, contract) ->
        if curParticipant == Just newParticipant
          then cur
          else
            case Map.lookup curLabel contract of
              Just (stmts, Nothing) ->
                case last stmts of
                  Label lastLabel ->
                    let newContract = contract
                          & Map.insert curLabel (init stmts, Just lastLabel)
                          & Map.insert lastLabel ([SetParticipant newParticipant], Nothing)
                    in (Just newParticipant, lastLabel, newContract)
                  _ ->
                    error "Change of participant with no preceding label."
              Just (_, Just _) ->
                error "Invalid transition state"
              Nothing ->
                (Just newParticipant, curLabel, contract & Map.insert curLabel ([SetParticipant newParticipant], Nothing))


    addStatement :: Statement -> State (Maybe ValueRef, ExecutionPoint, Map ExecutionPoint ([Statement], Maybe ExecutionPoint)) ()
    addStatement stmt =
      modify $ \(curParticipant, curLabel, contract) ->
        let newContract = case Map.lookup curLabel contract of
              Just (stmts, exitPoints) ->
                Map.insert curLabel (stmts <> [stmt], exitPoints) contract
              Nothing ->
                contract
        in (curParticipant, curLabel, newContract)


var :: String -> ValueRef
var = Variable . BSC.pack

parseName :: SExpr -> String
parseName = \case
  Atom name ->
    name
  List [Atom "quote", Atom name] ->
    name
  unknown ->
    error $ "Invalid name expression: " <> show unknown

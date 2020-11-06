{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Void
import Text.SExpression as SExpr
import Text.Megaparsec hiding (Label, State)

import Language.PlutusTx.Prelude (ByteString)
import Language.PlutusTx.AssocMap (Map)
import qualified Language.PlutusTx.AssocMap as Map
import qualified Ledger.Crypto as Ledger

import Types

type SExprString = String

parseContract :: SExprString -> SExprString -> SExprString -> Either (ParseErrorBundle String Void) ([Statement], Map ByteString Value)
parseContract headerStr bodyStr variableMapStr = do
  headerSexpr <- parse (parseSExpr def) "header" headerStr
  bodySexpr <- parse (parseSExpr def) "body" bodyStr
  variableMapSexpr <- parse (parseSExpr def) "variable map" variableMapStr
  let headerStatements = parseContractHeader headerSexpr
  let datatypeMap = Map.empty
  let bodyStatements = parseContractBody datatypeMap bodySexpr
  let variableMap = parseVariableMap datatypeMap variableMapSexpr
  pure (headerStatements <> bodyStatements, variableMap)

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


parseContractBody :: DatatypeMap -> SExpr -> [Statement]
parseContractBody datatypes = \case
  List (Atom "@body":content) ->
    join $ parseStatement <$> content

  unknown ->
    error $ "Unknown body format: " <> show unknown

  where
    parseStatement = \case
      List [Atom "@label", Atom name] ->
        [Label $ BSC.pack name]

      List [Atom "consensus:set-participant", roleName] ->
        [SetParticipant (var $ parseName roleName)]

      List [Atom "expect-deposited", Atom amountName] ->
        [ExpectDeposited (var amountName)]

      List [Atom "expect-withdrawn", Atom roleName, Atom amountName] ->
        [ExpectWithdrawn (var roleName) (var amountName)]

      List [Atom "def", Atom variableName, List [Atom "λ", List [Atom argName], body]] ->
        case parseContractBody datatypes body of
          statements ->
            [DefineFunction (BSC.pack variableName) (BSC.pack argName) statements]
          --unknown ->
          --  error $ "Invalid right hand side of definition: " <> show unknown

      List [Atom "def", Atom variableName, sexpr] ->
        case parseExpression sexpr of
          [expression] ->
            [Define (BSC.pack variableName) expression]
          unknown ->
            error $ "Invalid right hand side of definition: " <> show unknown

      List [Atom "require!", Atom variableName] ->
        [Require $ var variableName]

      List [Atom "return", List [Atom typeName]] ->
        [Return $ var typeName]

      unknown ->
        error $ "Unknown statement in contract body: " <> show unknown

    parseExpression = \case
      List [Atom "expect-published", variableName] ->
        [ExpectPublished (BSC.pack $ parseName variableName)]

      List [Atom "@app", Atom "isValidSignature", Atom roleName, Atom digestVariableName, Atom signatureVariableName] ->
        [IsValidSignature (var roleName) (var digestVariableName) (var signatureVariableName)]

      unknown ->
        error $ "Unknown expression in contract body: " <> show unknown


    parseName = \case
      Atom name ->
        name
      List [Atom "quote", Atom name] ->
        name
      unknown ->
        error $ "Invalid name expression: " <> show unknown

    var = Variable . BSC.pack

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

generateContract :: [Statement] -> GlowContract
generateContract statements =
  let (_, _, contract) = execState (traverse processStatement statements) (Nothing, "begin", Map.singleton "begin" ([], Nothing))
  in contract
  where
    processStatement stmt = do
      case stmt of
        SetParticipant newParticipant -> setParticipant newParticipant
        _ -> addStatement stmt

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
                          & Map.insert lastLabel ([Label lastLabel, SetParticipant newParticipant], Nothing)
                    in (Just newParticipant, lastLabel, newContract)
                  _ ->
                    error "Change of participant with no preceding label."
              _ ->
                error "Invalid transition state."

    addStatement :: Statement -> State (Maybe ValueRef, ExecutionPoint, Map ExecutionPoint ([Statement], Maybe ExecutionPoint)) ()
    addStatement stmt =
      modify $ \(curParticipant, curLabel, contract) ->
        (curParticipant, curLabel, case Map.lookup curLabel contract of
          Just (stmts, exitPoints) ->
            Map.insert curLabel (stmts <> [stmt], exitPoints) contract
          _ ->
            error $ "Invalid current label: " <> BSC.unpack curLabel)


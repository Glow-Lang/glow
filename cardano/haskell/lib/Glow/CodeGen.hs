{- ORMOLU_DISABLE -}
-- | FIXME:
-- This currently relies on haskell types to generate corresponding Glow types.
-- It should be implemented with Glow types as the source of truth instead.
-- When we start getting Glow Cardano ready for production,
-- this module will be overhauled.
-- Hence we can treat this as a blackbox for now.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Glow.CodeGen where

import Data.Char
import Control.Lens hiding (List)
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import qualified PlutusTx.AssocMap as P
import Ledger (Script)

data Gerbil
  = RecordType Text [(Text, Text)]
  | SumType Text [(Text, [Text])]
  | Synonym Text Text
  deriving (Eq, Ord)

instance Pretty Gerbil where
  pretty (RecordType name fields) =
    parens $ "define-type" <+> pretty name <> line <>
      indent 2 (parens $ "Record" <> line <>
        indent 2 (vsep ((\(fieldName, fieldType) -> pretty (fieldName <> ":" ) <+> brackets (pretty fieldType)) <$> fields)))

  pretty (SumType name constructors) =
    parens $ "define-type" <+> pretty name <> line <>
      indent 2 (parens $ "Sum" <> line <>
      indent 2 (vsep ((\(consName, consTypes) -> (pretty $ consName <> ":" ) <+>
        case consTypes of
          [] -> "Unit"
          [single] -> pretty single
          multiple -> parens (hsep $ "Tuple":(pretty <$> multiple))) <$> constructors)))

  pretty (Synonym synonym actual) =
    parens $ "define-type" <+> pretty synonym <+> pretty actual

data Definition = Definition
  { typeName :: Name
  , gerbilCode :: Gerbil
  }

instance Pretty Definition where
  pretty (Definition {..}) = vsep
    [ ";" <+> pretty (fromMaybe "" $ namePackage typeName)
    , ";" <+> pretty (fromMaybe "" $ nameModule typeName) <> ":" <> pretty (nameBase typeName)
    , pretty gerbilCode
    , ""
    ]

-- | FIXME: Recursive types need `delay-type` wrapper around references to themselves.
writeSchemeDefinitions :: FilePath -> [Name] -> DecsQ
writeSchemeDefinitions moduleName names = do
  (definitions, _) <- execStateT makePrototypeDefinitions ([], names)
  let gerbilModule = show $ vsep $
        [ "(export #t)"
        , ""
        , "(import"
        , "  :clan/poo/object :clan/poo/number :clan/poo/mop :clan/poo/type"
        , "  ../poo-extensions)"
        , ""
        , ""
        ] <> (pretty <$> definitions)
  let functionName = mkName $ "storeSchemeDefinitions_" <> moduleName
      outputArgName = mkName "outputDir"
      functionBody = NormalB $
        AppE (AppE (VarE 'writeFile)
          (InfixE (Just $ VarE outputArgName) (VarE 'mappend) (Just $ LitE $ StringL $ "/" <> moduleName <> ".ss")))
          (LitE $ StringL gerbilModule)
  pure [FunD functionName [Clause [VarP outputArgName] functionBody []]]

type GeneratorState r = StateT ([Definition], [Name]) Q r

makePrototypeDefinitions :: GeneratorState ()
makePrototypeDefinitions = do
  curName <- popName
  case curName of
    Nothing -> pure ()
    Just name -> do
      done <- gets fst
      unless (name `elem` fmap typeName done) $ do
        info <- lift $ reify name
        case info of
          TyConI (TySynD synonym [] actual) -> do
            typeText <- showType actual
            addDefinition $ Definition
              { typeName = name
              , gerbilCode = Synonym (T.pack $ nameBase synonym) typeText
              }
          _ -> do
            datatype <- lift $ reifyDatatype name
            case datatypeCons datatype of
              -- TODO: figure out a better way to handle newtypes
              [singleCons] -> do
                fields <- productFields singleCons
                addDefinition $ Definition
                  { typeName = datatypeName datatype
                  , gerbilCode = RecordType
                      (T.pack $ nameBase $ constructorName singleCons)
                      fields
                  }
              multipleCons -> do
                fields <- traverse sumField multipleCons
                addDefinition $ Definition
                  { typeName = datatypeName datatype
                  , gerbilCode = SumType
                      (T.pack $ nameBase name)
                      fields
                  }
      makePrototypeDefinitions

popName :: GeneratorState (Maybe Name)
popName = do
  (done, todo) <- get
  case todo of
    n:ns -> do
      put (done, ns)
      pure $ Just n
    [] ->
      pure Nothing

addName :: Name -> GeneratorState ()
addName name = do
  definitions <- gets fst
  unless (name `elem` primitives || name `elem` fmap typeName definitions) $
    modify $ \(done, todo) ->
      (done, snoc todo name)

addDefinition :: Definition -> GeneratorState ()
addDefinition definition = do
  modify $ \(done, todo) ->
    (definition:done, todo)

primitives :: [Name]
primitives =
  [ ''BS.ByteString
  , ''LBS.ByteString
  , ''String
  , ''Integer
  , ''Int
  , ''Bool
  , ''()
  , ''[]
  , ''Set
  , ''Map
  , ''P.Map
  , ''Script
  , ''Maybe
  ]

sumField :: ConstructorInfo -> GeneratorState (Text, [Text])
sumField consInfo = do
  typeNames <- traverse showType (constructorFields consInfo)
  pure (T.pack . nameBase $ constructorName consInfo, typeNames)

productFields :: ConstructorInfo -> GeneratorState [(Text, Text)]
productFields consInfo = do
  typeNames <- traverse showType (constructorFields consInfo)
  pure $ case constructorVariant consInfo of
    RecordConstructor names -> zip (T.pack . nameBase <$> names) typeNames
    _ -> zip (over _head toLower . head . T.words <$> typeNames) typeNames

-- TODO:: Support multi-parameter types
showType :: Type -> GeneratorState Text
showType type' =
  case type' of
    VarT _ ->
      pure "Any"
    ConT name -> do
      addName name
      pure $ T.pack (nameBase name)
    AppT (ConT name) (VarT _) -> do
      addName name
      pure $ T.pack (nameBase name)
    AppT (ConT name1) (ConT name2) -> do
      addName name1
      addName name2
      pure $ "(" <> T.pack (nameBase name1) <> " " <> T.pack (nameBase name2) <> ")"
    AppT (AppT (ConT name1) (ConT name2)) t -> do
      addName name1
      addName name2
      typeText <- showType t
      pure $ "(" <> T.pack (nameBase name1) <> " " <> T.pack (nameBase name2) <> " -> " <> typeText <> ")"
    AppT (ConT name) x -> do
      addName name
      typeText <- showType x
      pure $ T.pack (nameBase name) <> " " <> typeText
    AppT ListT x -> do
      typeText <- showType x
      pure $ "(List " <> typeText  <> ")"
    AppT (AppT (TupleT _) x) y -> do
      typeTextX <- showType x
      typeTextY <- showType y
      pure $ "(Tuple " <> typeTextX <> " " <> typeTextY <> ")"
    _ -> error $ "Unsupported type: " <> show type'

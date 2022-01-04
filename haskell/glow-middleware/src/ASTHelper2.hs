{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ASTHelper2 where

import GHC.Read

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List as L

import Control.Monad
import Data.Functor
import Data.Function

import Data.Char (isAlphaNum,isDigit)

import Data.SCargot
import Data.SCargot.Repr
import Data.SCargot.Parse

import Text.Parsec

import Debug.Trace

import Data.SCargot.Print

import Data.Foldable

import Data.Maybe

-- import 

-- import Data.SCargot.Language.Basic
-- type GlowSExpr = 

isAtomChar :: Char -> Bool
isAtomChar c = isAlphaNum c
  || c == '-' || c == '*' || c == '/'
  || c == '+' || c == '<' || c == '>' || c == '#'
  || c == '=' || c == '!' || c == '?' 


-- pToken :: ParsecT T.Text a Identity T.Text
fixConstructor :: T.Text -> T.Text
fixConstructor "" = ""
fixConstructor "#f" = "HashF"
fixConstructor x | T.head x == '"' = x
                 | otherwise = toCamelCase x

mbConstructor :: T.Text -> Maybe T.Text
mbConstructor "" = Nothing
mbConstructor "#f" = Just "hash-f"
mbConstructor x | T.head x == '"' = Nothing
                | isDigit (T.head x) = Nothing
                | otherwise = Just $ x


toCamelCase :: T.Text -> T.Text
toCamelCase x =
  T.concat $ fmap T.toTitle (T.splitOn "-" x )


pToken = T.pack <$> many1 (satisfy isAtomChar)


pString = T.pack <$> do
   char '"'
   x <- many (satisfy (\x -> x /= '"'))
   char '"'
   return $ "\"" <> x <> "\""


   
basicParser :: SExprParser T.Text (SExpr T.Text)
basicParser = mkParser $
   pString <|> pToken 

-- evalList :: SExpr T.Text -> SExpr T.Text
-- evalList (SCons (SAtom "temp-internal-list") t) = t
--   where
--     mkList :: SExpr T.Text -> SExpr T.Text
--     mkList SNil = 
-- evalList x = x

forceToWellF :: RichSExpr T.Text -> WellFormedSExpr T.Text
forceToWellF (RSList l) = WFSList (fmap forceToWellF l)
forceToWellF (RSDotted l x) = trace ("!!!" ++ show l) $ WFSList ((fmap forceToWellF l) ++ [WFSAtom x])
forceToWellF (RSAtom x) = WFSAtom x

data SExpr' = L' [SExpr'] | A' T.Text deriving (Show)


toSExpr' :: WellFormedSExpr T.Text -> SExpr' 
toSExpr' (WFSList l) = L' (fmap toSExpr' l) 
toSExpr' (WFSAtom x) = A' x

(<&>>) :: Functor f => f a -> (a -> b) -> f a
(<&>>) x _ = x 


  
test :: IO ()
test = do
  f <- T.replace "'()" "(list)" <$> T.replace "]" ")" <$> T.replace "[" " ( list " <$> TIO.readFile "../../agda/exampleAST"

  TIO.putStrLn f
  -- case (((decode basicParser f) <&> (fmap (forceToWellF . toRich)) )) of
  --   Left e -> putStrLn e
  --   Right x ->  TIO.putStrLn $ encode (basicPrint id) (fromWellFormed <$> x)
  case (((decode basicParser f) >>= (mapM toWellFormed))) of
    Left e -> putStrLn e
    Right x -> do
               let dettectedTokens =  catMaybes $ mbConstructor <$> (nub $ concat (fmap Data.Foldable.toList x))
               -- putStrLn "-------"
               -- putStrLn (show dettectedTokens)
               -- putStrLn "-------"
               -- mapM (TIO.putStrLn) dettectedTokens
               -- putStrLn "-------"
               -- TIO.putStrLn $ encode (basicPrint id) (fromWellFormed <$> x)
               let x' = fmap (fmap fixConstructor) x
               -- putStrLn $ show (toVanilaASTL' $ fmap (toSExpr') x')
               TIO.putStrLn $ generateAgdaCode (toVanilaASTL' $ fmap (toSExpr') x')
 
allowedTokens :: [T.Text]
allowedTokens = ["statement-with-attribute","attribute","identifier","list-expression","function-definition","hash-f","body-expression","expression-statement","deposit-expression","numeric-literal","value-definition","call-expression","arguments","string-literal","publish-statement","if-expression","equality-expression","withdraw-expression"]

printTokensCode :: IO  ()
printTokensCode = do
               putStrLn "-------"
               mapM (TIO.putStrLn . ("| " <>) . toCamelCase) allowedTokens
               putStrLn "-------"

printTokensAgdaCode :: IO  ()
printTokensAgdaCode = do
               putStrLn "-------"
               mapM (TIO.putStrLn . ("  " <>) . (<> " : Token")) allowedTokens
               putStrLn "-------"
  




data Token =
    StatementWithAttribute
  | Attribute
  | Identifier
  | ListExpression
  | FunctionDefinition
  | HashF
  | BodyExpression
  | ExpressionStatement
  | DepositExpression
  | NumericLiteral
  | ValueDefinition
  | CallExpression
  | Arguments
  | StringLiteral
  | PublishStatement
  | IfExpression
  | EqualityExpression
  | WithdrawExpression
 deriving (Show , Read)

toProperToken :: Token -> T.Text
toProperToken x =
  fromJust $ find ((== (T.pack (show x))) . toCamelCase) allowedTokens

data VanillaAST =
    E [VanillaAST]
  | L [VanillaAST]
  | S T.Text
  | I Int
  | T Token

  
 deriving (Show , Read)

toVanilaAST :: SExpr' -> VanillaAST
toVanilaAST (A' x) | T.head x == '"' = S (T.drop 1 $ T.dropEnd 1 $ x)
                   | isDigit (T.head x) = I (read $ (T.unpack x))
                   | otherwise = T (read $ (T.unpack x))
toVanilaAST (L' (A' "List" : xs)) = L (fmap toVanilaAST xs)
toVanilaAST (L' xs) = E (fmap toVanilaAST xs)

toVanilaASTL :: [SExpr'] -> VanillaAST
toVanilaASTL = L . fmap toVanilaAST

toVanilaASTL' :: [SExpr'] -> VanillaAST
toVanilaASTL' = head . fmap toVanilaAST

toAgdaList :: [VanillaAST] -> T.Text
toAgdaList [] = "[]"
toAgdaList xs = " ( " <> (T.intercalate " ∷ " $ fmap generateAgdaCode xs)  <> " ∷ [] ) " 

generateAgdaSExpr :: VanillaAST -> WellFormedSExpr T.Text
generateAgdaSExpr = \case
    E xs -> WFSList (WFSAtom "𝓔" : (WFSAtom $ toAgdaList xs) : [])
    L xs -> WFSList (WFSAtom "𝓛" : (WFSAtom $ toAgdaList xs) : [])
    S t ->  WFSList ([WFSAtom "𝑺" , WFSAtom ("\"" <> t <> "\"")])
    I i -> WFSList ([WFSAtom "𝑰" , WFSAtom (T.pack $ show i)])
    T t -> WFSList ([WFSAtom "𝑻" , WFSAtom (toProperToken t)])

generateAgdaCode :: VanillaAST ->  T.Text
generateAgdaCode =
   encodeOne (basicPrint id) . fromWellFormed  . generateAgdaSExpr

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ASTHelper where

import GHC.Read

import Data.Text as T
import Data.Text.IO as TIO

import Data.List as L

import Control.Monad
import Data.Functor
import Data.Function

import Data.SCargot.Parse





-- TODO: imporve the tretment of string literals!! (low priority...)

splitOnAllAndKeep :: [ T.Text ] ->  T.Text -> [T.Text]
splitOnAllAndKeep splits input =
    foldM (\t sp -> L.intersperse sp (T.splitOn sp t)) input splits

toCamelCase :: T.Text -> T.Text
toCamelCase x =
  T.concat $ fmap T.toTitle (T.splitOn "-" x )
  
fixConstructor :: T.Text -> T.Text
fixConstructor "" = ""
fixConstructor x | T.head x == '"' = x
                 | otherwise = toCamelCase x
                 
  -- if head x == '"'

-- do not splits inside strings
groupStringLiterals :: [T.Text] -> [T.Text]
groupStringLiterals [] = []
groupStringLiterals (x : xs) | (not $ T.null x) && T.head x == '"' && (not (T.last x == '"')) = h x xs
                             | otherwise = (x : groupStringLiterals xs)
  where
    h :: T.Text -> [T.Text] -> [T.Text]
    h a [] = [a]
    h a (x : xs) | (not $ T.null x) && T.last x == '"'  = (a <> x) : groupStringLiterals xs
                 | otherwise = h (a <> x) xs
                 
fixConstructors :: T.Text -> T.Text
fixConstructors t =
  T.unwords $ L.filter (not . T.null) $
   (T.words t 
      >>= splitOnAllAndKeep ["[","]","(",")"])
      & groupStringLiterals
      <&> fixConstructor
      
test :: IO ()
test = do
  f <- TIO.readFile "exampleAST"
  TIO.putStr  (fixConstructors f)


type Op = String

data Literal =
 BooleanLiteral String |
 NumericLiteral Int |
 StringLiteral String

data Type =
   TypeName String
 | TypeVar String
 | TypeTuple [String]
 -- | TypeRecord (Assocof String Type)
 | TypeWithAttribute Attribute Type

data Identifier = Identifier String 

data Attribute =
  Attribute Identifier Expression


data Expression = Expression

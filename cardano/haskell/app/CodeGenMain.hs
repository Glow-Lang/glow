{- ORMOLU_DISABLE -}
{-# LANGUAGE TemplateHaskell #-}

import Ledger.Tx
import Options.Applicative
import System.Directory

import Glow.CodeGen
import Glow.Client
import Glow.Client.Types
import Glow.Types

$(writeSchemeDefinitions "transaction" [''Tx])
$(writeSchemeDefinitions "data" [''GlowDatum, ''GlowRedeemer])
$(writeSchemeDefinitions "client" [''CreateParams, ''MoveParams, ''RawCreateParams, ''RawMoveParams])

main :: IO ()
main = do
  codeGenOptions <- execParser options
  storeSchemeDefinitions $ outputDirectory codeGenOptions

storeSchemeDefinitions :: FilePath -> IO ()
storeSchemeDefinitions outputDir = do
  createDirectoryIfMissing True outputDir
  storeSchemeDefinitions_transaction outputDir
  storeSchemeDefinitions_data outputDir
  storeSchemeDefinitions_client outputDir

data CodeGenOptions = CodeGenOptions
  { outputDirectory :: FilePath }

options :: ParserInfo CodeGenOptions
options = info codeGen
  (fullDesc <> progDesc "Generate module of Scheme types from Haskell types")

codeGen :: Parser CodeGenOptions
codeGen = CodeGenOptions
  <$> strOption (long "output" <> short 'o' <> help "Directory to output generated Scheme modules")

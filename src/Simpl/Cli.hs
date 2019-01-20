{-# LANGUAGE LambdaCase #-}
module Simpl.Cli where

import Options.Applicative

data CliCmd
  = Compile { fileName :: String
            , target :: Target }
    deriving (Show)

data Target = NativeTarget | IRTarget
  deriving (Show)

targetReader :: ReadM Target
targetReader = maybeReader $ \case
  "native" -> Just NativeTarget
  "llvmir" -> Just IRTarget
  _ -> Nothing

cliProgram :: ParserInfo CliCmd
cliProgram = info compileCommand mempty

compileCommand :: Parser CliCmd
compileCommand = Compile <$>
  strArgument (metavar "file_name")
  <*> option targetReader (short 'T' <> long "target" <> value NativeTarget)

runCliParser :: IO CliCmd
runCliParser = execParser cliProgram

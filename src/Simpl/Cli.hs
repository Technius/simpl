{-# LANGUAGE LambdaCase #-}
module Simpl.Cli where

import Options.Applicative

data CliCmd
  = Compile { fileName :: String -- ^ Name of the source file
            , target :: Target   -- ^ Compilation target
            , dumpIR :: Bool     -- ^ Whether the LLVM IR should be printed on stderr
            }
    deriving (Show)

data Target = NativeTarget | IRTarget
  deriving (Show)

targetReader :: ReadM Target
targetReader = maybeReader $ \case
  "native" -> Just NativeTarget
  "llvmir" -> Just IRTarget
  _ -> Nothing

cliProgram :: ParserInfo CliCmd
cliProgram = info (helper <*> compileCommand) mempty

compileCommand :: Parser CliCmd
compileCommand = Compile <$>
  strArgument (metavar "file_name")
  <*> option targetReader (short 'T' <> long "target" <> value NativeTarget)
  <*> switch (long "dump-ir")

runCliParser :: IO CliCmd
runCliParser = execParser cliProgram

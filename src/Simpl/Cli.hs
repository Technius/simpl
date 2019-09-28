{-# LANGUAGE LambdaCase #-}
module Simpl.Cli where

import Options.Applicative

data CliCmd
  = Compile { -- | Name of the source file
              sourceFile :: String
              -- | Name of the output file
            , outputFile :: String
              -- | Compilation target
            , target :: Target
              -- | Whether the LLVM IR should be printed on stderr
            , dumpIR :: Bool
              -- | Whether diagnostics should be printed at runtime
            , enableDiagnostics :: Bool
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
  strArgument (metavar "source_file")
  <*> strOption (short 'o' <> long "output" <> value "a.out")
  <*> option targetReader (short 'T' <> long "target" <> value NativeTarget)
  <*> switch (long "dump-ir")
  <*> switch (long "diagnostics")

runCliParser :: IO CliCmd
runCliParser = execParser cliProgram

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Simplc where

import qualified Data.ByteString as B
import Control.Exception (displayException, SomeException)
import Control.Exception.Safe (handle)
import Control.Monad (forM_)
import Control.Monad.Except
import Data.List (find)
import Simpl.Ast
import Simpl.Compiler
import qualified Simpl.Cli as Cli
import qualified Simpl.Parser as Parser
import LLVM.Target (withHostTargetMachine)
import LLVM.Module (File(File), withModuleFromAST, writeObjectToFile, moduleLLVMAssembly)
import LLVM.Internal.Context
import LLVM.Analysis (verify)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Text.Megaparsec (runParser, parseErrorPretty')
import System.Exit (exitFailure)

-- | Error monad
type EIO a = ExceptT [String] IO a

main :: IO ()
main = do
  options <- Cli.runCliParser
  appRes <- runExceptT (appLogic options)
  case appRes of
    Left errLines -> do
      forM_ errLines putStrLn
      exitFailure
    Right _ -> pure ()

appLogic :: Cli.CliCmd -> EIO ()
appLogic options = do
  ast <- readSourceFile (Cli.fileName options)
  codegen ast

codegen :: SourceFile Expr -> EIO ()
codegen srcFile@(SourceFile _ decls) =
  case find isMain decls of
    Just _ -> do
      liftIO $ putStrLn "Running compiler pipeline"
      let pipeline = ExceptT (fullCompilerPipeline srcFile)
      llvmMod <- withExceptT (pure . ("Error: " ++) . show) pipeline
      outputModule llvmMod
    _ -> throwError $ ["No main function found"]
  where
    isMain = \case
      DeclFun n _ _ _ -> n == "main"
      _ -> False

readSourceFile :: String -> EIO (SourceFile Expr)
readSourceFile filepath = do
  sourceContents <- liftIO $ TextIO.readFile filepath
  let res = runParser (Parser.sourceFile (Text.pack filepath)) filepath sourceContents
  case res of
    Left err ->
      throwError $ pure (parseErrorPretty' sourceContents err)
    Right ast -> pure ast

handleExceptions :: EIO () -> EIO ()
handleExceptions = handle $ \(e :: SomeException) ->
  handler (displayException e)
  where
    handler :: String -> EIO ()
    handler err = throwError $ ["An error occurred:"] ++ lines err

outputModule llvmMod = handleExceptions . liftIO $
  withHostTargetMachine $ \target ->
    withContext $ \ctx ->
      withModuleFromAST ctx llvmMod $ \mod' -> do
        putStrLn "Generated IR"
        moduleLLVMAssembly mod' >>= B.putStr
        putStrLn "Verifying module"
        verify mod'
        putStrLn "Generating code"
        writeObjectToFile target (File "sample.o") mod'

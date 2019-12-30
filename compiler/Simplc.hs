{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simplc where

import qualified Data.ByteString as B

import Control.Exception.Safe (catches, Handler(..), MonadCatch, MonadThrow)
import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (find)
import Simpl.Ast
import Simpl.Compiler
import Simpl.CompilerOptions
import qualified Simpl.Cli as Cli
import qualified Simpl.Parser as Parser
import LLVM.Target (withHostTargetMachineDefault)
import qualified LLVM.AST
import LLVM.Module ( File(File), withModuleFromAST
                   , writeObjectToFile , moduleLLVMAssembly, linkModules, Module)
import LLVM.Context
import LLVM.Analysis (verify)
import LLVM.Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Text.Megaparsec (runParser, parseErrorPretty')
import System.Exit (exitFailure)
import System.IO (stderr)

-- | Monad for handling CLI
newtype CliM a = CliM { unCliM :: ReaderT Cli.CliCmd (ExceptT [String] IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Cli.CliCmd
           , MonadError [String]
           , MonadIO
           , MonadCatch
           , MonadThrow )

runCliM :: Cli.CliCmd -> CliM a -> IO (Either [String] a)
runCliM options = runExceptT . flip runReaderT options . unCliM

main :: IO ()
main = do
  options <- Cli.runCliParser
  appRes <- runCliM options appLogic
  case appRes of
    Left errLines -> do
      forM_ errLines putStrLn
      exitFailure
    Right _ -> pure ()

appLogic :: CliM ()
appLogic = do
  options <- ask
  ast <- readSourceFile (Cli.sourceFile options)
  codegen ast

codegen :: SourceFile SourcedExpr -> CliM ()
codegen srcFile@(SourceFile _ decls) =
  case find isMain decls of
    Just _ -> do
      compilerOpts <- asks $ \cliOpts -> defaultCompilerOpts
            { enableDiagnostics = Cli.enableDiagnostics cliOpts
            , dumpJoinIR = Cli.dumpJoinIR cliOpts }
      liftIO $ putStrLn "Running compiler pipeline"
      let pipeline = ExceptT (fullCompilerPipeline compilerOpts srcFile)
      programAst <- CliM . lift $ withExceptT (pure . ("Error: " ++) . show) pipeline
      dumpIR <- asks Cli.dumpIR
      outputName <- asks Cli.outputFile
      handleExceptions . liftIO $
        withContext $ \cr ->
          buildRuntime cr $ \runtimeMod ->
            buildModule programAst dumpIR $ \programMod -> do
              linkModules programMod runtimeMod
              outputModule programMod outputName
    _ -> throwError $ ["No main function found"]
  where
    isMain = \case
      DeclFun n _ _ _ -> n == "main"
      _ -> False

readSourceFile :: String -> CliM (SourceFile SourcedExpr)
readSourceFile filepath = do
  sourceContents <- liftIO $ TextIO.readFile filepath
  let res = runParser (Parser.sourceFile (Text.pack filepath)) filepath sourceContents
  case res of
    Left err ->
      throwError $ pure (parseErrorPretty' sourceContents err)
    Right ast -> pure ast

handleExceptions :: CliM () -> CliM ()
handleExceptions = flip catches $
  [ Handler (\(EncodeException s) -> handler s)
  , Handler (\(DecodeException s) -> handler s)
  , Handler (\(LinkException s) -> handler s)
  , Handler (\(VerifyException s) -> handler s) ]
  where
    handler :: String -> CliM ()
    handler err = throwError $ ["An error occurred:"] ++ lines err

buildModule :: LLVM.AST.Module -> Bool -> (Module -> IO a) -> IO a
buildModule modAst dumpIR cont =
  withContext $ \ctx ->
    withModuleFromAST ctx modAst $ \llvmMod -> do
      when dumpIR $
        moduleLLVMAssembly llvmMod >>= B.hPutStr stderr
      putStrLn "Verifying AST"
      verify llvmMod
      putStrLn "AST verified successfully"
      cont llvmMod

outputModule :: Module -> FilePath -> IO ()
outputModule llvmMod outputName =
  withHostTargetMachineDefault $ \target ->
    writeObjectToFile target (File outputName) llvmMod

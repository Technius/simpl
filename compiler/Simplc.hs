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
import qualified Simpl.Cli as Cli
import qualified Simpl.Parser as Parser
import LLVM.Target (withHostTargetMachine)
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
  ast <- readSourceFile (Cli.fileName options)
  codegen ast

codegen :: SourceFile Expr -> CliM ()
codegen srcFile@(SourceFile _ decls) =
  case find isMain decls of
    Just _ -> do
      liftIO $ putStrLn "Running compiler pipeline"
      let pipeline = ExceptT (fullCompilerPipeline srcFile)
      programAst <- CliM . lift $ withExceptT (pure . ("Error: " ++) . show) pipeline
      handleExceptions . liftIO $
        withContext $ \cr ->
          buildRuntime cr $ \runtimeMod ->
            buildModule programAst $ \programMod -> do
              linkModules programMod runtimeMod
              moduleLLVMAssembly programMod >>= B.putStr
              outputModule programMod
    _ -> throwError $ ["No main function found"]
  where
    isMain = \case
      DeclFun n _ _ _ -> n == "main"
      _ -> False

readSourceFile :: String -> CliM (SourceFile Expr)
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

buildModule :: LLVM.AST.Module -> (Module -> IO a) -> IO a
buildModule modAst cont =
  withContext $ \ctx ->
    withModuleFromAST ctx modAst $ \llvmMod -> do
      putStrLn "Verifying AST"
      verify llvmMod
      putStrLn "AST verified successfully"
      cont llvmMod

outputModule :: Module -> IO ()
outputModule llvmMod =
  withHostTargetMachine $ \target ->
    writeObjectToFile target (File "sample.o") llvmMod

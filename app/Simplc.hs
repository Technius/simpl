{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Simplc where

import qualified Data.ByteString as B
import Control.Exception
import Control.Monad (forM_)
import Data.List (find)
import Simpl.Ast
import Simpl.Compiler
import qualified Simpl.Parser as Parser
import LLVM.Target (withHostTargetMachine)
import LLVM.Module (File(File), withModuleFromAST, writeObjectToFile, moduleLLVMAssembly)
import LLVM.Internal.Context
import LLVM.Analysis (verify)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Text.Megaparsec (runParser, parseErrorPretty')

main :: IO ()
main = do
  progAstRes <- readSourceFile "sample.spl"
  case progAstRes of
    Just ast -> codegen ast
    Nothing -> pure ()

codegen :: SourceFile Expr -> IO ()
codegen srcFile@(SourceFile _ decls) =
  case find isMain decls of
    Just _ -> do
      putStrLn "Running compiler pipeline"
      modRes <- fullCompilerPipeline srcFile
      case modRes of
        Left err -> putStrLn $ "Error: " ++ show err
        Right llvmMod -> outputModule llvmMod
    _ -> putStrLn "No main function found" >> pure ()
  where
    isMain = \case
      DeclFun n _ _ -> n == "main"
      _ -> False

readSourceFile :: String -> IO (Maybe (SourceFile Expr))
readSourceFile filepath = do
  sourceContents <- TextIO.readFile filepath
  let res = runParser (Parser.sourceFile (Text.pack filepath)) filepath sourceContents
  case res of
    Left err -> do
      putStrLn $ parseErrorPretty' sourceContents err
      pure Nothing
    Right ast -> pure (Just ast)

handleErrors :: IO () -> IO ()
handleErrors action = catch action $ \(e :: SomeException) -> do
  putStrLn "An error occured: "
  forM_ (lines (displayException e)) putStrLn

outputModule llvmMod =
  withHostTargetMachine $ \target ->
    withContext $ \ctx ->
      withModuleFromAST ctx llvmMod $ \mod' -> do
        putStrLn "Generated IR"
        moduleLLVMAssembly mod' >>= B.putStr
        putStrLn "Verifying module"
        verify mod'
        putStrLn "Generating code"
        writeObjectToFile target (File "sample.o") mod'

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Simpl.Compiler where
import Control.Monad.Except
import Control.Monad.State
import Data.Set (Set)
import Data.Text.Prettyprint.Doc (Pretty, pretty)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text (Text)
import qualified Data.Map as Map
import System.IO (stderr, hPrint)

import qualified LLVM.AST as LLVM
import qualified LLVM.Module as LLVMM
import LLVM.Context

import Simpl.Annotation (unannotate, Fields(..))
import Simpl.Ast
import Simpl.AstToJoinIR
import Simpl.Backend.Codegen (runCodegen)
import Simpl.CompilerOptions
import Simpl.SymbolTable
import Simpl.Type (Type)
import Simpl.Typecheck (TypeError, runTypecheck, checkType, withExtraVars, Typecheck)
import Paths_simpl_lang

-- | Main error type, aggregating all error types.
data CompilerErr
  = ErrTypecheck TypeError
  | ErrAnalysis
  deriving (Show)

instance Pretty CompilerErr where
  pretty = \case
    ErrTypecheck err -> PP.hsep ["Type error:", pretty err]
    ErrAnalysis -> "Static analysis error <TODO>"

-- | Monad for handling compiler
newtype CompilerMonad e a = MkCompilerMonad
  { unCompiler :: StateT (SymbolTable e) (ExceptT CompilerErr IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (SymbolTable e)
           , MonadError CompilerErr
           , MonadIO)

runCompiler :: SymbolTable e -> CompilerMonad e a -> IO (Either CompilerErr a)
runCompiler symTab
  = runExceptT
  . flip evalStateT symTab
  . unCompiler

-- | Builds the SimPL runtime
buildRuntime :: Context -> (LLVMM.Module -> IO a) -> IO a
buildRuntime ctx cont = do
  runtimeSourcePath <- getDataFileName "runtime/runtime.ll"
  LLVMM.withModuleFromLLVMAssembly ctx (LLVMM.File runtimeSourcePath) cont

-- | Compiles a SimPL source file
fullCompilerPipeline :: CompilerOpts
                     -> SourceFile (AnnExpr '[ 'ExprPos ])
                     -> IO (Either CompilerErr LLVM.Module)
fullCompilerPipeline options srcFile@(SourceFile _name decls) =
  runCompiler (buildSymbolTable srcFile) $ do
    symTable <- get
    let newSTTypecheck = sequence $ symTabMapExprs tycheckFuns symTable
    typedSymTable <- MkCompilerMonad . lift . withExceptT ErrTypecheck $
          liftEither (runTypecheck symTable newSTTypecheck)
    let jSymTable = astToJoinIR typedSymTable
    let srcCode = unlines [show (pretty (unannotate <$> d)) | d <- decls]

    when (dumpJoinIR options) $
      liftIO $ forM_ (Map.toList $ symTabFuns jSymTable) $ \(name, (_, args, ty, expr)) -> do
        hPrint stderr (pretty name <> " " <> pretty args <> " :: " <> pretty ty)
        hPrint stderr (pretty (unannotate expr))

    pure $ runCodegen options srcCode jSymTable
  where
    tycheckFuns :: (Set Text, [(Text, Type)], Type, SourcedExpr)
                -> (Set Text, [(Text, Type)], Type, Typecheck '[ 'ExprPos] (AnnExpr '[ 'ExprType, 'TCType, 'ExprPos]))
    tycheckFuns (tvars, params, ty, expr) =
      (tvars, params, ty, withExtraVars params (checkType ty expr))

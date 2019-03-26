{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Simpl.Compiler where
import Control.Monad.Except
import Control.Monad.State
import Data.Text.Prettyprint.Doc (pretty)

import qualified LLVM.AST as LLVM
import qualified LLVM.Module as LLVMM
import LLVM.Context

import Simpl.Ast
import Simpl.AstToJoinIR
import Simpl.Backend.CodegenJoin (runCodegen)
import Simpl.CompilerOptions
import Simpl.SymbolTable
import Simpl.Typing (TypeError, runTypecheck, checkType, withExtraVars)
import Simpl.JoinIR (unannotate)
import Paths_simpl_lang

-- | Main error type, aggregating all error types.
data CompilerErr
  = ErrTypecheck TypeError
  | ErrAnalysis
  deriving (Show)

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
fullCompilerPipeline :: CompilerOpts -> SourceFile Expr -> IO (Either CompilerErr LLVM.Module)
fullCompilerPipeline options srcFile@(SourceFile _name decls) =
  runCompiler (buildSymbolTable srcFile) $ do
    symTable <- get
    let tycheckFuns (params, ty, expr) = (params, ty, withExtraVars params (checkType ty expr))
    let newSTTypecheck = sequence $ symTabMapExprs tycheckFuns symTable
    typedSymTable <- MkCompilerMonad . lift . withExceptT ErrTypecheck $
          liftEither (runTypecheck symTable newSTTypecheck)
    let jSymTable = astToJoinIR typedSymTable
    -- TODO: Add compiler flag to dump JoinIR
    -- liftIO $ forM_ (symTabFuns jSymTable) $ \(args, ty, expr) -> do
    --   print (pretty args <> pretty " :: " <> pretty ty)
    --   print (pretty (unannotate expr))
    let srcCode = unlines [show (pretty d) | d <- decls]
    pure $ runCodegen options srcCode jSymTable

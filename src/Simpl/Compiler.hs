{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simpl.Compiler where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified LLVM.AST as LLVM

import Simpl.Analysis
import Simpl.Ast
import Simpl.Codegen (generateLLVM)
import Simpl.Typing (TypeError, runTypecheck, checkType, typeToUtype)

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

fullCompilerPipeline :: SourceFile Expr -> IO (Either CompilerErr LLVM.Module)
fullCompilerPipeline srcFile@(SourceFile _name decls) =
  runCompiler (buildSymbolTable srcFile) $ do
    forM_ decls $ \case
      DeclFun _fname ty expr -> do
        symTable <- get
        _ <- MkCompilerMonad . lift . withExceptT ErrTypecheck $
          liftEither (runTypecheck symTable (checkType expr (typeToUtype ty)))
        pure ()
      _ -> pure ()
    gets (runReader (generateLLVM decls))

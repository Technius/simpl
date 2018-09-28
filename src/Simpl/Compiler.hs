{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simpl.Compiler where

import Control.Monad.State

import Control.Monad.Except
import Data.Functor.Identity
import qualified LLVM.AST as LLVM

import Simpl.Analysis
import Simpl.Ast
import Simpl.Codegen (generateLLVM)
import Simpl.Typing (TypeError, runTypecheck, inferType)

-- | Main error type, aggregating all error types.
data CompilerErr
  = ErrTypecheck TypeError
  | ErrAnalysis
  deriving (Show)

-- | Monad for handling compiler
newtype CompilerMonad e a = MkCompilerMonad
  { unCompiler :: StateT (SymbolTable e) (ExceptT CompilerErr Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (SymbolTable e)
           , MonadError CompilerErr)

runCompiler :: SymbolTable e -> CompilerMonad e a -> Either CompilerErr a
runCompiler symTab
  = runIdentity
  . runExceptT
  . flip evalStateT symTab
  . unCompiler

fullCompilerPipeline :: Expr -> CompilerMonad Expr LLVM.Module
fullCompilerPipeline program = do
  symTable <- get
  _ty <- MkCompilerMonad . lift . withExceptT ErrTypecheck $
    liftEither (runTypecheck symTable (inferType program))
  pure $ generateLLVM program

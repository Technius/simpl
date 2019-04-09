{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Simpl.AstToJoinIR
Description : Provides a function to normalize SimPL AST, transforming it into
              JoinIR.
-}
module Simpl.AstToJoinIR
  ( astToJoinIR
  ) where

import Control.Monad.Supply
import Control.Monad.Reader hiding (guard)
import Data.Functor.Foldable (Fix(..), unfix)
import Data.Functor.Identity
import Data.Text (Text)
import Data.String (fromString)

import Simpl.Annotation hiding (AnnExprF, AnnExpr)
import Simpl.Ast (Type)
import Simpl.SymbolTable
import qualified Simpl.Ast as A
import qualified Simpl.JoinIR.Syntax as J

-- * Public API

astToJoinIR :: SymbolTable (A.AnnExpr Type) -> SymbolTable (J.AnnExpr '[ 'ExprType])
astToJoinIR = runTransform transformTable

-- * Transformation Monad

newtype TransformT m a =
  TransformT { unTransform :: ReaderT (SymbolTable (A.AnnExpr Type)) (SupplyT Int m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (SymbolTable (A.AnnExpr Type))
           , MonadFreshVar)

type Transform = TransformT Identity

type MonadFreshVar = MonadSupply Int

varSupply :: [Int]
varSupply = [0..]

runTransformT :: Monad m => TransformT m a -> SymbolTable (A.AnnExpr Type) -> m a
runTransformT m table
  = fmap fst
  . flip runSupplyT varSupply
  . flip runReaderT table
  . unTransform
  $ m

runTransform :: Transform a -> SymbolTable (A.AnnExpr Type) -> a
runTransform m table = runIdentity (runTransformT m table)

-- | Generates a fresh name using the given prefix and lookup function
freshName :: (MonadReader (SymbolTable (A.AnnExpr Type)) m, MonadFreshVar m)
         => Text -- ^ Prefix
         -> (Text -> SymbolTable (A.AnnExpr Type) -> Maybe a) -- ^ Lookup function
         -> m Text
freshName prefix lookupFun = do
  next <- (prefix <>) . fromString . show <$> supply
  asks (lookupFun next) >>= \case
    Nothing -> pure next
    Just _  -> freshName prefix lookupFun

freshVar, freshLabel :: (MonadReader (SymbolTable (A.AnnExpr Type)) m, MonadFreshVar m) => m Text
-- | Generate a fresh variable name
freshVar = freshName "var" symTabLookupVar
-- | Generate a fresh join label
freshLabel = freshName "join" symTabLookupFun

-- * Private utility functions

makeJexpr :: Type
          -> J.JExprF (J.AnnExpr '[ 'ExprType])
          -> J.AnnExpr '[ 'ExprType]
makeJexpr ty = Fix . addField (withType ty) . toAnnExprF

astType :: A.AnnExpr Type -> Type
astType = A.annGetAnn . unfix

-- * ANF Transformation

-- | Perform ANF transformation on the given symbol table
transformTable :: (MonadReader (SymbolTable (A.AnnExpr Type)) m, MonadFreshVar m)
               => m (SymbolTable (J.AnnExpr '[ 'ExprType]))
transformTable = do
  table <- ask
  symTabTraverseExprs (\(args, ty, expr) -> (args, ty, transformExpr expr)) table

-- | Perform ANF transformation on the given expression
transformExpr :: (MonadReader (SymbolTable (A.AnnExpr Type)) m, MonadFreshVar m)
              => A.AnnExpr Type
              -> m (J.AnnExpr '[ 'ExprType])
transformExpr expr = anfTransform expr (pure . makeJexpr (astType expr) . J.JVal)

-- | Perform ANF transformation on the branch, afterwards handling control flow.
transformBranch :: (MonadReader (SymbolTable (A.AnnExpr Type)) m, MonadFreshVar m)
                => J.ControlFlow (J.AnnExpr '[ 'ExprType]) -- ^ Control flow handler
                -> A.Branch (A.AnnExpr Type) -- ^ Branches
                -> m (J.JBranch (J.AnnExpr '[ 'ExprType]))
transformBranch cf (A.BrAdt adtName argNames expr) = do
  jexpr <- anfTransform expr (pure . makeJexpr (astType expr) . J.JVal)
  pure $ J.BrAdt adtName argNames (J.Cfe jexpr cf)


-- | Main ANF transformation logic. Given the SimPL AST, this function will
-- normalize the AST, and then it will feed the final JValue into the given
-- continuation to produce the resulting JoinIR AST.
anfTransform :: (MonadReader (SymbolTable (A.AnnExpr Type)) m, MonadFreshVar m)
             => A.AnnExpr Type -- ^ Expression to translate
             -> (J.JValue -> m (J.AnnExpr '[ 'ExprType])) -- ^ Continuation
             -> m (J.AnnExpr '[ 'ExprType])
anfTransform (Fix (A.AnnExprF ty exprf)) cont = case exprf of
  A.Lit lit -> cont (J.JLit lit)
  A.Var name -> cont (J.JVar name)
  A.Let name bindExpr next ->
    anfTransform bindExpr $ \bindVal ->
      makeJexpr (A.annGetAnn (unfix bindExpr)) . J.JLet name bindVal <$>
        local (symTabInsertVar name ty) (anfTransform next cont)
  A.BinOp op left right ->
    anfTransform left $ \jleft ->
      anfTransform right $ \jright -> do
        name <- freshVar
        makeJexpr ty . J.JApp name (J.CBinOp op) [jleft, jright] <$>
          local (symTabInsertVar name ty) (cont (J.JVar name))
  A.If guard trueBr falseBr ->
    anfTransform guard $ \jguard -> do
      lbl <- freshLabel
      trueBr'  <- anfTransform trueBr (pure . makeJexpr (astType trueBr) . J.JVal)
      falseBr' <- anfTransform falseBr (pure . makeJexpr (astType falseBr) . J.JVal)
      name <- freshVar
      let jmp = J.JJump lbl
      let guardTy = A.annGetAnn (unfix guard)
      let guardCfe = makeJexpr guardTy (J.JVal jguard)
      let cfe = J.Cfe guardCfe (J.JIf (J.Cfe trueBr' jmp) (J.Cfe falseBr' jmp))
      -- TODO: Make JJoin node placement more efficient
      makeJexpr ty . J.JJoin lbl name cfe <$>
        local (symTabInsertVar name ty) (cont (J.JVar name))
  A.Case branches expr ->
    anfTransform expr $ \jexpr -> do
      lbl <- freshLabel
      let jexprTy = A.annGetAnn (unfix expr)
      jbranches <- traverse (transformBranch (J.JJump lbl)) branches
      let jexprCfe = J.Cfe (makeJexpr jexprTy (J.JVal jexpr)) (J.JCase jbranches)
      name <- freshVar
      -- TODO: Make JJoin node placement more efficient
      makeJexpr ty . J.JJoin lbl name jexprCfe <$>
        local (symTabInsertVar name ty) (cont (J.JVar name))
  A.Cons ctorName args ->
    collectArgs args $ \argVals -> do
      varName <- freshVar
      makeJexpr ty . J.JApp varName (J.CCtor ctorName) argVals <$>
        local (symTabInsertVar varName ty) (cont (J.JVar varName))
  A.App funcName args ->
    collectArgs args $ \argVals -> do
      varName <- freshVar
      makeJexpr ty . J.JApp varName (J.CFunc funcName) argVals <$>
        local (symTabInsertVar varName ty) (cont (J.JVar varName))
  A.Cast expr numTy ->
    anfTransform expr $ \jexpr -> do
      varName <- freshVar
      makeJexpr ty . J.JApp varName (J.CCast numTy) [jexpr] <$>
        local (symTabInsertVar varName ty) (cont (J.JVar varName))
  A.Print expr ->
    anfTransform expr $ \jexpr -> do
      varName <- freshVar
      makeJexpr ty . J.JApp varName J.CPrint [jexpr] <$>
        local (symTabInsertVar varName ty) (cont (J.JVar varName))
  A.FunRef name -> do
    varName <- freshVar
    makeJexpr ty . J.JApp varName (J.CFunRef name) [] <$>
      local (symTabInsertVar varName ty) (cont (J.JVar varName))

-- | Normalize each expression in sequential order, and then run the
-- continuation with the expression values.
collectArgs :: (MonadReader (SymbolTable (A.AnnExpr Type)) m, MonadFreshVar m)
            => [A.AnnExpr Type] -- ^ Argument expressions
            -> ([J.JValue] -> m (J.AnnExpr '[ 'ExprType])) -- ^ Continuation
            -> m (J.AnnExpr '[ 'ExprType])
collectArgs = go []
  where
    go vals [] mcont = mcont (reverse vals)
    go vals (x:xs) mcont = anfTransform x $ \v -> go (v:vals) xs mcont

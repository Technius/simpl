{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Simpl.AstToJoinIR
Description : Provides a function to normalize SimPL AST, transforming it into
              JoinIR.
-}
module Simpl.AstToJoinIR
  ( astToJoinIR
  ) where

import Control.Monad.Reader hiding (guard)
import Data.Functor.Foldable (Fix(..), unfix)
import Data.Functor.Identity
import Data.Text (Text)
import Data.String (fromString)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

import Simpl.Annotation
import Simpl.SymbolTable
import qualified Simpl.Ast as A
import qualified Simpl.JoinIR.Syntax as J
import Simpl.Type (Type, TypeF(TyBox, TyVar))
import Simpl.Typecheck (literalType)
import Simpl.Util.Supply
import qualified Simpl.Util.Stream as Stream

-- * Public API

astToJoinIR :: HasType flds => SymbolTable (A.AnnExpr flds) -> SymbolTable (J.AnnExpr '[ 'ExprType])
astToJoinIR table = runTransform transformTable (defaultCtx table)

-- * Transformation Monad

data BoxedVal = Boxed | Unboxed deriving (Show, Eq, Ord)

data TransformCtx fields = TransformCtx
  { tcSymTab :: SymbolTable (A.AnnExpr fields)
  , tcJoinLabels :: Set Text
  , tcBoxStatus :: Map Text BoxedVal
  }

defaultCtx :: SymbolTable (A.AnnExpr flds) -> TransformCtx flds
defaultCtx table = TransformCtx
  { tcSymTab = table
  , tcJoinLabels = Set.empty
  , tcBoxStatus = Map.empty }

insertVar :: Text -> Type -> TransformCtx flds -> TransformCtx flds
insertVar name ty ctx = ctx
  { tcSymTab = symTabInsertVar name ty (tcSymTab ctx)
  , tcBoxStatus = Map.insert name (boxedVal ty) (tcBoxStatus ctx)
  }

newtype TransformT fields m a =
  TransformT { unTransform :: ReaderT (TransformCtx fields) (SupplyT Int m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadFreshVar)

deriving instance Monad m => MonadReader (TransformCtx flds) (TransformT flds m)

type Transform fields = TransformT fields Identity

type MonadFreshVar = MonadSupply Int

varSupply :: Stream.Stream Int
varSupply = Stream.iterate (+1) 0

runTransformT :: Monad m => TransformT flds m a -> TransformCtx flds -> m a
runTransformT m table
  = fmap fst
  . flip runSupplyT varSupply
  . flip runReaderT table
  . unTransform
  $ m

runTransform :: Transform flds a -> TransformCtx flds -> a
runTransform m table = runIdentity (runTransformT m table)

-- | Generates a fresh name using the given prefix and lookup function
freshName :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
         => Text -- ^ Prefix
         -> (Text -> TransformCtx flds -> Maybe a) -- ^ Lookup function
         -> m Text
freshName prefix lookupFun = do
  next <- (prefix <>) . fromString . show <$> supply
  asks (lookupFun next) >>= \case
    Nothing -> pure next
    Just _  -> freshName prefix lookupFun

freshVar, freshLabel :: HasType flds => (MonadReader (TransformCtx flds) m, MonadFreshVar m) => m Text
-- | Generate a fresh variable name
freshVar = freshName "var" (\v ctx -> symTabLookupVar v (tcSymTab ctx))
-- | Generate a fresh join label
freshLabel = freshName "join" (\v ctx -> if Set.member v (tcJoinLabels ctx) then Just v else Nothing)

-- * Private utility functions

makeJexpr :: Type
          -> J.JExprF (J.AnnExpr '[ 'ExprType])
          -> J.AnnExpr '[ 'ExprType]
makeJexpr ty = Fix . addField (withType ty) . toAnnExprF

astType :: HasType flds => A.AnnExpr flds -> Type
astType = getType . unfix

getJvalueType :: MonadReader (TransformCtx flds) m => J.JValue -> m Type
getJvalueType = \case
  J.JVar n -> asks (fromJust . symTabLookupVar n . tcSymTab)
  J.JLit l -> pure . Fix $ literalType l

-- | Get boxed type. Left is unboxed, right is boxed.
boxedType :: Type -> Either Type Type
boxedType = \case
  t@(Fix (TyVar _)) -> Right t
  Fix (TyBox t) -> Right t
  t -> Left t

isBoxed :: Type -> Bool
isBoxed t = case boxedType t of { Left _ -> False; Right _ -> True }

boxedVal :: Type -> BoxedVal
boxedVal t = if isBoxed t then Boxed else Unboxed

rebindBoxing :: (HasType flds, MonadFreshVar m, MonadReader (TransformCtx flds) m)
             => J.JValue     -- ^ Variable name
             -> Type     -- ^ Variable type
             -> BoxedVal -- ^ Whether to ensure boxed or unboxed
             -> m (J.JValue, J.AnnExpr '[ 'ExprType] -> J.AnnExpr '[ 'ExprType])
rebindBoxing val ty b = do
  let create ty' action = do
        name <- case val of { J.JVar n -> pure n; _ -> freshVar }
        local (insertVar name ty') (pure (J.JVar name, makeJexpr ty' . J.JApp name action [val]))
  case (boxedType ty, b) of
    (Right ty', Unboxed) -> create ty' J.CUntag
    (Left ty', Boxed) -> create (Fix (TyBox ty')) J.CTag
    _ -> pure (val, id)

-- * ANF Transformation

-- | Perform ANF transformation on the given symbol table
transformTable :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
               => m (SymbolTable (J.AnnExpr '[ 'ExprType]))
transformTable = do
  table <- asks tcSymTab
  symTabTraverseExprs (\(tvars, args, ty, expr) -> (tvars, args, ty, transformExpr expr)) table

-- | Perform ANF transformation on the given expression
transformExpr :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
              => A.AnnExpr flds
              -> m (J.AnnExpr '[ 'ExprType])
transformExpr expr = anfTransform expr (pure . makeJexpr (astType expr) . J.JVal)

-- | Perform ANF transformation on the branch, afterwards handling control flow.
transformBranch :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
                => J.ControlFlow (J.AnnExpr '[ 'ExprType]) -- ^ Control flow handler
                -> A.Branch (A.AnnExpr flds) -- ^ Branches
                -> m (J.JBranch (J.AnnExpr '[ 'ExprType]))
transformBranch cf (A.BrAdt adtName argNames expr) = do
  jexpr <- anfTransform expr (pure . makeJexpr (astType expr) . J.JVal)
  pure $ J.BrAdt adtName argNames (J.Cfe jexpr cf)


-- | Main ANF transformation logic. Given the SimPL AST, this function will
-- normalize the AST, and then it will feed the final JValue into the given
-- continuation to produce the resulting JoinIR AST.
anfTransform :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
             => A.AnnExpr flds -- ^ Expression to translate
             -> (J.JValue -> m (J.AnnExpr '[ 'ExprType])) -- ^ Continuation
             -> m (J.AnnExpr '[ 'ExprType])
anfTransform (Fix ae) cont = let ty = getType ae in case annGetExpr ae of
  A.Lit lit -> cont (J.JLit lit)
  A.Var name -> cont (J.JVar name)
  A.Let name bindExpr next ->
    anfTransform bindExpr $ \bindVal ->
      makeJexpr (getType (unfix bindExpr)) . J.JLet name bindVal <$>
        local (insertVar name ty) (anfTransform next cont)
  A.BinOp op left right ->
    anfTransform left $ \jleft ->
      anfTransform right $ \jright -> do
        name <- freshVar
        makeJexpr ty . J.JApp name (J.CBinOp op) [jleft, jright] <$>
          local (insertVar name ty) (cont (J.JVar name))
  A.If guard trueBr falseBr ->
    anfTransform guard $ \jguard -> do
      lbl <- freshLabel
      trueBr'  <- anfTransform trueBr (pure . makeJexpr (astType trueBr) . J.JVal)
      falseBr' <- anfTransform falseBr (pure . makeJexpr (astType falseBr) . J.JVal)
      name <- freshVar
      let jmp = J.JJump lbl
      let guardTy = getType (unfix guard)
      let guardCfe = makeJexpr guardTy (J.JVal jguard)
      let cfe = J.Cfe guardCfe (J.JIf (J.Cfe trueBr' jmp) (J.Cfe falseBr' jmp))
      -- TODO: Make JJoin node placement more efficient
      makeJexpr ty . J.JJoin lbl name cfe <$>
        local (insertVar name ty) (cont (J.JVar name))
  A.Case branches expr ->
    anfTransform expr $ \jexpr -> do
      lbl <- freshLabel
      let jexprTy = astType expr
      jbranches <- traverse (transformBranch (J.JJump lbl)) branches
      let jexprCfe = J.Cfe (makeJexpr jexprTy (J.JVal jexpr)) (J.JCase jbranches)
      name <- freshVar
      -- TODO: Make JJoin node placement more efficient
      makeJexpr ty . J.JJoin lbl name jexprCfe <$>
        local (insertVar name ty) (cont (J.JVar name))
  A.Cons ctorName args ->
    collectArgs args $ \argVals -> do
      -- TODO: find and box polymorphic args
      varName <- freshVar
      makeJexpr ty . J.JApp varName (J.CCtor ctorName) argVals <$>
        local (insertVar varName ty) (cont (J.JVar varName))
  A.App funcName args ->
    collectArgs args $ \argVals -> do
      varName <- freshVar
      (_, funcArgs, funcRetTy, _) <- asks (fromJust . symTabLookupStaticFun funcName . tcSymTab)
      argTys <- traverse getJvalueType argVals
      tuples <- sequence [rebindBoxing val aTy (boxedVal faTy)
                         | (((_, faTy), val), aTy) <- funcArgs `zip` argVals `zip` argTys]
      let (argVals', boxConvArgs_) = unzip tuples
      let boxConvArgs = foldl (.) id boxConvArgs_
      let ty' = if isBoxed funcRetTy then Fix (TyBox ty) else ty
      boxConvArgs . makeJexpr ty' . J.JApp varName (J.CFunc funcName) argVals' <$>
        local (insertVar varName ty') (cont (J.JVar varName))
  A.Cast expr numTy ->
    anfTransform expr $ \jexpr -> do
      varName <- freshVar
      makeJexpr ty . J.JApp varName (J.CCast numTy) [jexpr] <$>
        local (insertVar varName ty) (cont (J.JVar varName))
  A.Print expr ->
    anfTransform expr $ \jval -> do
      varName <- freshVar
      valTy <- getJvalueType jval
      (jval', boxConv) <- rebindBoxing jval valTy Unboxed
      boxConv . makeJexpr ty . J.JApp varName J.CPrint [jval'] <$>
        local (insertVar varName ty) (cont (J.JVar varName))
  A.FunRef name -> do
    varName <- freshVar
    makeJexpr ty . J.JApp varName (J.CFunRef name) [] <$>
      local (insertVar varName ty) (cont (J.JVar varName))

-- | Normalize each expression in sequential order, and then run the
-- continuation with the expression values.
collectArgs :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
            => [A.AnnExpr flds] -- ^ Argument expressions
            -> ([J.JValue] -> m (J.AnnExpr '[ 'ExprType])) -- ^ Continuation
            -> m (J.AnnExpr '[ 'ExprType])
collectArgs = go []
  where
    go vals [] mcont = mcont (reverse vals)
    go vals (x:xs) mcont = anfTransform x $ \v -> go (v:vals) xs mcont

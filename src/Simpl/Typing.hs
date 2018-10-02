{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Simpl.Typing where

import Control.Applicative (liftA2)
import Control.Monad (when, foldM, forM_)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks, local)
import Control.Monad.Except (ExceptT, MonadError, lift, runExceptT, throwError)
import Control.Unification
import Control.Unification.IntVar
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Functor.Foldable (Fix(..), unfix, cata)
import Data.Text (Text)

import Simpl.Ast
import Simpl.Analysis

type UVar = IntVar
type UType = UTerm TypeF UVar

type TCExpr = AnnExpr UType
type TypedExpr = AnnExpr Type

data TypeError
  = TyErrOccurs UVar UType
  | TyErrMismatch (TypeF UType) (TypeF UType) -- ^ Expected, actual
  | TyErrArgCount Int Int [Type] -- ^ Expected count, actual count, expected arg types
  | TyErrNoSuchCtor Text
  | TyErrAmbiguousType UType
  deriving (Show)

instance Fallible TypeF UVar TypeError where
  occursFailure = TyErrOccurs
  mismatchFailure = TyErrMismatch

newtype Typecheck a = Typecheck
  { unTypecheck ::
      ReaderT (SymbolTable Expr) (
        ExceptT TypeError (
          IntBindingT TypeF Identity)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (SymbolTable Expr)
             , MonadError TypeError)

-- | Unify expected with actual type
unifyTy :: UType -> UType -> Typecheck UType
unifyTy t1 t2 = Typecheck . lift $ unify t1 t2

mkMetaVar :: Typecheck UType
mkMetaVar = Typecheck . lift . lift $ UVar <$> freeVar

forceBindings :: UType -> Typecheck UType
forceBindings v = Typecheck . lift $ applyBindings v

literalType :: Literal -> TypeF a
literalType = \case
  LitBool _ -> TyBool
  LitDouble _ -> TyDouble

-- | Annotate every AST node with a unification meta variable
attachExprMetaVar :: Expr -> Typecheck TCExpr
attachExprMetaVar = cataM (\e -> Fix . flip AnnExprF e <$> mkMetaVar)

-- | Resolve all unification variables, returning a well-typed AST
tcExprToTypedExpr :: TCExpr -> Typecheck TypedExpr
tcExprToTypedExpr =
  cataM $ \(AnnExprF uty expr) ->
    utypeToType <$> forceBindings uty >>= \case
      Just ty -> pure . Fix $ AnnExprF ty expr
      Nothing -> throwError $ TyErrAmbiguousType uty

inferType :: Expr -> Typecheck TCExpr
inferType = cata $ \case
  Lit l -> pure $ annotate (Lit l) (UTerm (literalType l))
  Add x y -> doubleBinop Add x y
  Sub x y -> doubleBinop Sub x y
  Mul x y -> doubleBinop Mul x y
  Div x y -> doubleBinop Div x y
  If condM t1M t2M -> do
    cond <- condM
    _ <- unifyTy (extractTy cond) (UTerm TyBool)
    (t1, t2) <- liftA2 (,) t1M t2M
    resTy <- unifyTy (extractTy t1) (extractTy t2)
    pure $ annotate (If cond t1 t2) resTy
  Cons name argsM -> do
    args <- sequence argsM
    let argTys = extractTy <$> args
    ctorRes <- asks (symTabLookupCtor name)
    case ctorRes of
      Just (adtTy, Ctor _ ctorArgTys, _) -> do
        let conArgs = typeToUtype <$> ctorArgTys
        let numConArgs = length conArgs
        when (numConArgs /= length argTys) $
          throwError $ TyErrArgCount numConArgs (length argTys) ctorArgTys
        traverse_ (uncurry unifyTy) (zip conArgs argTys)
        pure $ annotate (Cons name args) (typeToUtype adtTy)
      Nothing -> throwError $ TyErrNoSuchCtor name
  Case branchMs valM -> do
    val <- valM
    let valTy = extractTy val
    branches <- sequence (sequence <$> branchMs)
    -- TODO: Insert bound variables into context
    forM_ branches $ \case
      BrAdt ctorName bindings _ ->
        asks (symTabLookupCtor ctorName) >>= \case
          Just (dataTy, Ctor _ ctorArgs, _) -> do
            when (length bindings /= length ctorArgs) $
              throwError $ TyErrArgCount (length ctorArgs) (length bindings) ctorArgs
            unifyTy valTy (typeToUtype dataTy)
          Nothing -> throwError $ TyErrNoSuchCtor ctorName
    let brTys = extractTy . branchGetExpr <$> branches
    resTy <- mkMetaVar
    annotate (Case branches val) <$> foldM unifyTy resTy brTys
  Let name valM nextM -> do
    val <- valM
    -- TODO: Hack, fix this
    case utypeToType (extractTy val) of
      Just ty -> do
        next <- local (symTabInsertVar name ty) nextM
        pure $ annotate (Let name val next) (extractTy next)
      Nothing -> throwError $ TyErrAmbiguousType (extractTy val)
  where
    annotate :: ExprF TCExpr -> UType -> TCExpr
    annotate expfTc ty = Fix $ AnnExprF ty expfTc

    extractTy :: TCExpr -> UType
    extractTy = annGetAnn . unfix

    doubleBinop :: (TCExpr -> TCExpr -> ExprF TCExpr)
                -> Typecheck TCExpr
                -> Typecheck TCExpr
                -> Typecheck TCExpr
    doubleBinop op xm ym = do
      (x, y) <- liftA2 (,) xm ym
      xTy <- unifyTy (extractTy x) (UTerm TyDouble)
      yTy <- unifyTy (extractTy y) xTy
      pure $ annotate (op x y) yTy

checkType :: Type -> Expr -> Typecheck TypedExpr
checkType ty expr = do
  typedExpr <- inferType expr
  _ <- unifyTy (annGetAnn . unfix $ typedExpr) (typeToUtype ty)
  tcExprToTypedExpr typedExpr

runTypecheck :: SymbolTable Expr -> Typecheck a -> Either TypeError a
runTypecheck ctx
  = runIdentity
  . evalIntBindingT
  . runExceptT
  . flip runReaderT ctx
  . unTypecheck

utypeToType :: UType -> Maybe Type
utypeToType ut = case ut of
  UTerm t -> Fix <$> traverse utypeToType t
  UVar _ -> Nothing

-- | Direct conversion of a [Type] to a [UType]. Does not instantiate variables.
typeToUtype :: Type -> UType
typeToUtype = cata $ \case
  TyDouble -> UTerm TyDouble
  TyBool -> UTerm TyBool
  TyAdt n -> UTerm (TyAdt n)

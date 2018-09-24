{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Simpl.Typing where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
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

data TypeError
  = TyErrOccurs UVar UType
  | TyErrMismatch (TypeF UType) (TypeF UType) -- ^ Expected, actual
  | TyErrArgCount Int Int [Type] -- ^ Expected count, actual count, expected arg types
  | TyErrNoSuchCtor Text
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

inferType :: Expr -> Typecheck UType
inferType e = mkMetaVar >>= checkType e

forceBindings :: UType -> Typecheck UType
forceBindings v = Typecheck . lift $ applyBindings v

checkType :: Expr -> UType -> Typecheck UType
checkType expr ty = case unfix expr of
    Lit l -> case l of
      LitBool _ -> unifyTy ty (UTerm TyBool)
      LitDouble _ -> unifyTy ty (UTerm TyDouble)
    Add t1 t2 -> doubleBinop t1 t2
    Sub t1 t2 -> doubleBinop t1 t2
    Mul t1 t2 -> doubleBinop t1 t2
    Div t1 t2 -> doubleBinop t1 t2
    If cond t1 t2 -> do
      _ <- checkType cond (UTerm TyBool)
      resTy <- inferType t1
      _ <- checkType t2 resTy
      unifyTy resTy ty
    Cons name args -> do
      argTys <- traverse inferType args
      ctorRes <- asks (symTabLookupCtor name)
      case ctorRes of
        Just (adtTy, Ctor _ ctorArgTys) -> do
          let conArgs = typeToUtype <$> ctorArgTys
          when (length conArgs /= length argTys) $
            throwError $ TyErrArgCount (length conArgs) (length argTys) ctorArgTys
          traverse_ (uncurry unifyTy) (zip conArgs argTys)
          unifyTy (typeToUtype adtTy) ty
        Nothing -> throwError $ TyErrNoSuchCtor name
  where
    doubleBinop x y = do
      ty1 <- checkType x (UTerm TyDouble)
      _   <- checkType y (UTerm TyDouble)
      unifyTy ty1 ty

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

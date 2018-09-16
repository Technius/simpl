{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Simpl.Typing where

import Control.Monad.Except (ExceptT, MonadError, lift, runExceptT)
import Control.Unification
import Control.Unification.IntVar
import Data.Functor.Identity
import Data.Functor.Foldable (Fix(..), unfix)

import Simpl.Ast

type UVar = IntVar
type UType = UTerm TypeF UVar

data TypeError
  = TyErrOccurs UVar UType
  | TyErrMismatch (TypeF UType) (TypeF UType) -- ^ Expected, actual
  deriving (Show)

instance Fallible TypeF UVar TypeError where
  occursFailure = TyErrOccurs
  mismatchFailure = TyErrMismatch

newtype Typecheck a = Typecheck
  { unTypecheck ::
      ExceptT TypeError (
        IntBindingT TypeF Identity) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError TypeError)

-- | Unify expected with actual type
unifyTy :: UType -> UType -> Typecheck UType
unifyTy t1 t2 = Typecheck $ unify t1 t2

mkMetaVar :: Typecheck UType
mkMetaVar = Typecheck . lift $ UVar <$> freeVar

inferType :: Expr -> Typecheck UType
inferType e = mkMetaVar >>= checkType e

forceBindings :: UType -> Typecheck UType
forceBindings v = Typecheck $ applyBindings v

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
  where
    doubleBinop x y = do
      ty1 <- checkType x (UTerm TyDouble)
      _   <- checkType y (UTerm TyDouble)
      unifyTy ty1 ty

runTypecheck :: Typecheck a -> Either TypeError a
runTypecheck
  = runIdentity
  . evalIntBindingT
  . runExceptT
  . unTypecheck

utypeToType :: UType -> Maybe Type
utypeToType ut = case ut of
  UTerm t -> Fix <$> traverse utypeToType t
  UVar _ -> Nothing

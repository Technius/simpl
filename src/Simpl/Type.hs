{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Simpl.Type where

import Control.Unification (Unifiable, zipMatch, UTerm)
import Control.Unification.IntVar (IntVar)
import Data.Functor.Foldable (Fix(Fix), para, cata)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Eq.Deriving (deriveEq1)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Show.Deriving (deriveShow1)

-- * Type System

-- | Numeric types
data Numeric = NumDouble -- ^ 64-bit floating point
             | NumInt -- ^ 64-bit signed integer
             | NumUnknown -- ^ Unknown (defaults to 64-bit floating point)
  deriving (Show, Eq)

instance Pretty Numeric where
  pretty = \case
    NumDouble -> "Double"
    NumInt -> "Int"
    NumUnknown -> "Num"


data TypeF a
  = TyNumber Numeric
  | TyBool
  | TyString
  | TyAdt Text [a]
  | TyFun [a] a
  | TyVar Text
  | TyBox a -- ^ Boxed polymorphic type
  deriving (Show, Functor, Foldable, Traversable)

type Type = Fix TypeF

$(deriveShow1 ''TypeF)
$(deriveEq1 ''TypeF)

instance Unifiable TypeF where
  zipMatch (TyNumber n) (TyNumber m) = case (n, m) of
    (NumUnknown, _) -> Just (TyNumber m)
    (_, NumUnknown) -> Just (TyNumber n)
    _ -> if n == m then Just (TyNumber n) else Nothing
  zipMatch (TyVar v1) (TyVar v2) =
    if v1 == v2 then Just (TyVar v1) else Nothing
  zipMatch TyBool TyBool = Just TyBool
  zipMatch TyString TyString = Just TyString
  zipMatch (TyAdt n1 tp1) (TyAdt n2 tp2) =
    if n1 == n2
    then Just $ TyAdt n1 (Right <$> zip tp1 tp2)
    else Nothing
  zipMatch (TyFun as1 r1) (TyFun as2 r2) =
    if length as1 == length as2 then
      -- TODO: Check by alpha-equivalence instead of raw equality
      Just $ TyFun (zipWith (curry Right) as1 as2) (Right (r1, r2))
    else
      Nothing
  zipMatch (TyBox t1) (TyBox t2) = Just $ TyBox (Right (t1, t2))
  zipMatch _ _ = Nothing

isComplexType :: TypeF a -> Bool
isComplexType = \case
  TyFun _ _ -> True
  _ -> False

functionTypeResult :: Type -> Type
functionTypeResult (Fix ty) = case ty of
  TyFun _ res -> functionTypeResult res
  _ -> Fix ty

getTypeVars :: Type -> Set Text
getTypeVars = cata $ \case
  TyBool -> Set.empty
  TyString -> Set.empty
  TyNumber _ -> Set.empty
  TyVar v -> Set.singleton v
  TyFun vparams vret -> Set.unions (vret:vparams)
  TyAdt _ vargs -> Set.unions vargs
  TyBox vs -> vs


instance Pretty Type where
  pretty = para go
    where
      wrapComplex (Fix t, ppr) =
        if isComplexType t then parens ppr else ppr
      go :: TypeF (Type, Doc ann) -> Doc ann
      go (TyNumber n) = pretty n
      go TyBool = "Bool"
      go TyString = "String"
      go (TyAdt n tparams) = pretty n <> hsep (snd <$> tparams)
      go (TyFun args res) =
        encloseSep mempty mempty " -> " (wrapComplex <$> args ++ [res])
      go (TyVar n) = pretty n
      go (TyBox b) = "#<" <> snd b <> ">"

-- | A universally quantified type.
data PolyType a = PolyType (Set Text) a -- ^ The type variables and the Type
  deriving (Functor, Show, Foldable, Traversable)

$(deriveShow1 ''PolyType)
$(deriveEq1 ''PolyType)

-- | Unification variable
type UVar = IntVar
-- | Unification type
type UType = UTerm TypeF UVar

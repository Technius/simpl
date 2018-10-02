{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Simpl.Ast where

import Control.Unification (Unifiable, zipMatch)
import Data.Functor.Foldable (Fix(Fix), project, para)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Text.Show.Deriving (deriveShow1)
import Data.Eq.Deriving (deriveEq1)

-- * AST Type

data ExprF a
  = Lit !Literal -- ^ Literal value
  | Add !a !a -- ^ Add (doubles)
  | Sub !a !a -- ^ Subtract (doubles)
  | Mul !a !a -- ^ Multiply (doubles)
  | Div !a !a -- ^ Divide (doubles)
  | If !a !a !a -- ^ If expression
  | Cons !Text ![a] -- ^ Construct ADT
  | Case [Branch a] a -- ^ Case deconstruction
  deriving (Functor, Foldable, Traversable, Show)

data Literal
  = LitDouble Double
  | LitBool Bool
  deriving (Eq, Show)

data Branch a = BrAdt Text [Text] a -- ^ Branch given constructor name, bindings, and expr
  deriving (Functor, Foldable, Traversable, Show)

branchGetExpr :: Branch a -> a
branchGetExpr = \case
  BrAdt _ _ e -> e

type Expr = Fix ExprF

isComplexExpr :: Expr -> Bool
isComplexExpr (Fix e) = case e of
  Lit l ->
    case l of
      LitDouble d -> d < 0
      _ -> False
  _ -> True

litDouble :: Double -> Expr
litDouble = Fix . Lit . LitDouble

litBool :: Bool -> Expr
litBool = Fix . Lit . LitBool

add :: Expr -> Expr -> Expr
add a b = Fix (Add a b)

sub :: Expr -> Expr -> Expr
sub a b = Fix (Sub a b)

mul :: Expr -> Expr -> Expr
mul a b = Fix (Mul a b)

div :: Expr -> Expr -> Expr
div a b = Fix (Div a b)

ifExpr :: Expr -> Expr -> Expr -> Expr
ifExpr cond t1 t2 = Fix (If cond t1 t2)

cons :: Text -> [Expr] -> Expr
cons name args = Fix (Cons name args)

branchAdt :: Text -> [Text] -> Expr -> Branch Expr
branchAdt = BrAdt

caseExpr :: [Branch Expr] -> Expr -> Expr
caseExpr branches val = Fix (Case branches val)

instance Pretty Literal where
  pretty (LitDouble d) = pretty d
  pretty (LitBool b) = pretty b

instance Pretty a => Pretty (Branch a) where
  pretty (BrAdt name bindings expr) =
    hsep (pretty <$> name : bindings) <+> "=>" <> softline <> pretty expr

instance Pretty Expr where
  pretty = para go
    where
      wrapComplex (x, px)
        | isComplexExpr x = parens px
        | otherwise = px
      binop op p1 p2 = wrapComplex p1 <+> op <+> wrapComplex p2
      go :: ExprF (Expr, Doc ann) -> Doc ann
      go (Lit l) = pretty l
      go (Add p1 p2) = binop "+" p1 p2
      go (Sub p1 p2) = binop "-" p1 p2
      go (Mul p1 p2) = binop "*" p1 p2
      go (Div p1 p2) = binop "/" p1 p2
      go (If cond (_, t1) (_, t2)) =
        hsep ["if", wrapComplex cond, "then", t1, "else", t2]
      go (Cons name args) =
        pretty name <+> hsep (wrapComplex <$> args)
      go (Case branches (_, valPpr)) =
        "case" <+> valPpr <+> "of" <> softline <> (hang 2 . hsep $
                                       pretty . fmap fst <$> branches)

$(deriveShow1 ''Branch)
$(deriveShow1 ''ExprF)

-- | An [Expr] annotated with some data.
data AnnExprF ann a = AnnExprF { annGetAnn :: ann, annGetExpr :: ExprF a }
  deriving (Functor, Foldable, Traversable)

type AnnExpr ann = Fix (AnnExprF ann)

-- | Catamorphism to a monadic value
cataM :: (Monad m, Traversable f) => (f a -> m a) -> Fix f -> m a
cataM f = (>>= f) . mapM (cataM f) . project

-- * Type System

data TypeF a
  = TyDouble
  | TyBool
  | TyAdt Text
  deriving (Show, Functor, Foldable, Traversable)

type Type = Fix TypeF

$(deriveShow1 ''TypeF)
$(deriveEq1 ''TypeF)

instance Unifiable TypeF where
  zipMatch TyDouble TyDouble = Just TyDouble
  zipMatch TyBool TyBool = Just TyBool
  zipMatch (TyAdt n1) (TyAdt n2) = if n1 == n2 then Just (TyAdt n1) else Nothing
  zipMatch _ _ = Nothing

instance Pretty Type where
  pretty = para go
    where
      go :: TypeF (Type, Doc ann) -> Doc ann
      go TyDouble = "Double"
      go TyBool = "Bool"
      go (TyAdt n) = pretty n

-- * Source File Types

data Constructor = Ctor { ctorGetName :: Text, ctorGetArgs :: [Type] }
  deriving (Show)

instance Pretty Constructor where
  pretty (Ctor name args) = hsep (pretty name : (pretty <$> args))

data Decl e
  = DeclFun Text Type e -- ^ A function declaration, in order of name, type, expression
  | DeclAdt Text [Constructor] -- ^ An algebraic data type declaration
  deriving (Show, Functor)

instance Pretty e => Pretty (Decl e) where
  pretty = \case
    DeclFun name ty expr ->
      hsep ["fun", pretty name, ":", pretty ty, "="] <> softline <> pretty expr
    DeclAdt name ctors ->
      hsep ["data", pretty name] <+> encloseSep "=" emptyDoc " | " (pretty <$> ctors)

data SourceFile e = SourceFile Text [Decl e]
  deriving (Show, Functor)

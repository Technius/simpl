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

data Numeric = NumDouble | NumInt | NumUnknown
  deriving (Show, Eq)

instance Pretty Numeric where
  pretty = \case
    NumDouble -> "Double"
    NumInt -> "Int"
    NumUnknown -> "Num"

-- * AST Type

data ExprF a
  = Lit !Literal -- ^ Literal value
  | Add !a !a -- ^ Add (doubles)
  | Sub !a !a -- ^ Subtract (doubles)
  | Mul !a !a -- ^ Multiply (doubles)
  | Div !a !a -- ^ Divide (doubles)
  | Lt !a !a -- ^ Less than (doubles)
  | Lte !a !a -- ^ Less than or equal to (doubles)
  | Equal !a !a -- ^ Equality (doubles)
  | If !a !a !a -- ^ If expression
  | Cons !Text ![a] -- ^ Construct ADT
  | Case [Branch a] !a -- ^ Case deconstruction
  | Let Text a a -- ^ Let expression
  | Var Text -- ^ Variable
  | App Text [a] -- ^ Function application
  | FunRef Text -- ^ Function reference
  | Cast !a !Numeric
  deriving (Functor, Foldable, Traversable, Show)

data Literal
  = LitDouble Double
  | LitInt Int
  | LitBool Bool
  deriving (Eq, Show)

data Branch a = BrAdt Text [Text] a -- ^ Branch given constructor name, bindings, and expr
  deriving (Functor, Foldable, Traversable, Show)

branchGetExpr :: Branch a -> a
branchGetExpr = \case
  BrAdt _ _ e -> e

branchGetBindings :: Branch a -> [Text]
branchGetBindings = \case
  BrAdt _ vars _ -> vars

type Expr = Fix ExprF

isComplexExpr :: Expr -> Bool
isComplexExpr (Fix e) = case e of
  Lit l ->
    case l of
      LitDouble d -> d < 0
      LitInt x -> x < 0
      _ -> False
  Var _ -> False
  _ -> True

litDouble :: Double -> Expr
litDouble = Fix . Lit . LitDouble

litInt :: Int -> Expr
litInt = Fix . Lit . LitInt

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

lt :: Expr -> Expr -> Expr
lt a b = Fix (Lt a b)

lte :: Expr -> Expr -> Expr
lte a b = Fix (Lte a b)

eq :: Expr -> Expr -> Expr
eq a b = Fix (Equal a b)

ifExpr :: Expr -> Expr -> Expr -> Expr
ifExpr cond t1 t2 = Fix (If cond t1 t2)

cons :: Text -> [Expr] -> Expr
cons name args = Fix (Cons name args)

branchAdt :: Text -> [Text] -> Expr -> Branch Expr
branchAdt = BrAdt

caseExpr :: [Branch Expr] -> Expr -> Expr
caseExpr branches val = Fix (Case branches val)

letExpr :: Text -> Expr -> Expr -> Expr
letExpr name val expr = Fix (Let name val expr)

castExpr :: Expr -> Numeric -> Expr
castExpr expr num = Fix (Cast expr num)

var :: Text -> Expr
var = Fix . Var

appExpr :: Text -> [Expr] -> Expr
appExpr name = Fix . App name

funRef :: Text -> Expr
funRef = Fix . FunRef

instance Pretty Literal where
  pretty (LitDouble d) = pretty d
  pretty (LitInt x) = pretty x
  pretty (LitBool b) = pretty b

instance Pretty a => Pretty (Branch a) where
  pretty (BrAdt name bindings expr) =
    hang 2 $ hsep (pretty <$> name : bindings) <+> "=>" <> softline <> pretty expr

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
      go (Lt p1 p2) = binop "<" p1 p2
      go (Lte p1 p2) = binop "<=" p1 p2
      go (Equal p1 p2) = binop "==" p1 p2
      go (If cond (_, t1) (_, t2)) =
        hsep ["if", wrapComplex cond, "then", t1, "else", t2]
      go (Cons name args) =
        pretty name <+> hsep (wrapComplex <$> args)
      go (Case branches (_, valPpr)) =
        hang 2 $ "case" <+> valPpr <+> "of" <> line <> (vsep $
                                                            pretty . fmap fst <$> branches)
      go (Let name (_, val) (_, expr)) =
        align $ hsep ["let", pretty name, "=", val, "in"] <> softline <> expr
      go (Var name) = pretty name
      go (App name args) = "@" <> pretty name <> encloseSep "(" ")" ", " (snd <$> args)
      go (FunRef name) = "#" <> pretty name
      go (Cast e n) = hsep [wrapComplex e, "as", pretty n]

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
  = TyNumber Numeric
  | TyBool
  | TyAdt Text
  | TyFun [a] a
  deriving (Show, Functor, Foldable, Traversable)

type Type = Fix TypeF

$(deriveShow1 ''TypeF)
$(deriveEq1 ''TypeF)

instance Unifiable TypeF where
  zipMatch (TyNumber n) (TyNumber m) = case (n, m) of
    (NumUnknown, _) -> Just (TyNumber m)
    (_, NumUnknown) -> Just (TyNumber n)
    _ -> if n == m then Just (TyNumber n) else Nothing
  zipMatch TyBool TyBool = Just TyBool
  zipMatch (TyAdt n1) (TyAdt n2) = if n1 == n2 then Just (TyAdt n1) else Nothing
  zipMatch (TyFun as1 r1) (TyFun as2 r2) =
    if length as1 == length as2 then
      Just $ TyFun (zipWith (curry Right) as1 as2) (Right (r1, r2))
    else
      Nothing
  zipMatch _ _ = Nothing

isComplexType :: TypeF a -> Bool
isComplexType = \case
  TyFun _ _ -> True
  _ -> False

instance Pretty Type where
  pretty = para go
    where
      wrapComplex (Fix t, ppr) =
        if isComplexType t then parens ppr else ppr
      go :: TypeF (Type, Doc ann) -> Doc ann
      go (TyNumber n) = pretty n
      go TyBool = "Bool"
      go (TyAdt n) = pretty n
      go (TyFun args res) =
        encloseSep mempty mempty " -> " (wrapComplex <$> args ++ [res])

-- * Source File Types

data Constructor = Ctor { ctorGetName :: Text, ctorGetArgs :: [Type] }
  deriving (Show)

instance Pretty Constructor where
  pretty (Ctor name args) = hsep (pretty name : (pretty <$> args))

data Decl e
  = DeclFun Text [(Text, Type)] Type e -- ^ A function declaration, in order of
                                       -- name, type, params, expression
  | DeclAdt Text [Constructor] -- ^ An algebraic data type declaration
  deriving (Show, Functor)

instance Pretty e => Pretty (Decl e) where
  pretty = \case
    DeclFun name params ty expr ->
      let params' = (\case (n, t) -> hsep [pretty n, ":", pretty t]) <$> params
          paramList = if null params then [] else [encloseSep "(" ")" ", " params']
      in hsep (["fun", pretty name] ++ paramList ++ [":", pretty ty, "="]) <> softline <> pretty expr
    DeclAdt name ctors ->
      hsep ["data", pretty name] <+> encloseSep "= " emptyDoc " | " (pretty <$> ctors)

data SourceFile e = SourceFile Text [Decl e]
  deriving (Show, Functor)

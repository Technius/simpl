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

-- | Binary operator types
data BinaryOp
  = Add -- ^ Add
  | Sub -- ^ Subtract
  | Mul -- ^ Multiply
  | Div -- ^ Divide
  | Lt -- ^ Less than
  | Lte -- ^ Less than or equal to
  | Equal -- ^ Equality
  deriving (Show, Eq)

instance Pretty BinaryOp where
  pretty = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Lt -> "<"
    Lte -> "<="
    Equal -> "=="

-- | Main AST type
data ExprF a
  = Lit !Literal -- ^ Literal value
  | BinOp !BinaryOp !a !a -- ^ Binary operator
  | If !a !a !a -- ^ If expression
  | Cons !Text ![a] -- ^ Construct ADT
  | Case [Branch a] !a -- ^ Case deconstruction
  | Let Text a a -- ^ Let expression
  | Var Text -- ^ Variable
  | App Text [a] -- ^ Function application
  | FunRef Text -- ^ Function reference
  | Cast !a !Numeric -- ^ Numeric value cast
  | Print !a -- ^ Print string (temporary)
  deriving (Functor, Foldable, Traversable, Show)

data Literal
  = LitDouble Double
  | LitInt Int
  | LitBool Bool
  | LitString Text
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

litString :: Text -> Expr
litString = Fix . Lit . LitString

binop :: BinaryOp -> Expr -> Expr -> Expr
binop op a b = Fix (BinOp op a b)

add :: Expr -> Expr -> Expr
add = binop Add

sub :: Expr -> Expr -> Expr
sub = binop Sub

mul :: Expr -> Expr -> Expr
mul = binop Mul

div :: Expr -> Expr -> Expr
div = binop Div

lt :: Expr -> Expr -> Expr
lt = binop Lt

lte :: Expr -> Expr -> Expr
lte = binop Lte

eq :: Expr -> Expr -> Expr
eq = binop Equal

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

printExpr :: Expr -> Expr
printExpr = Fix . Print

instance Pretty Literal where
  pretty (LitDouble d) = pretty d
  pretty (LitInt x) = pretty x
  pretty (LitBool b) = pretty b
  pretty (LitString s) = "\"" <> pretty (show s) <> "\""

instance Pretty a => Pretty (Branch a) where
  pretty (BrAdt name bindings expr) =
    hang 2 $ hsep (pretty <$> name : bindings) <+> "=>" <> softline <> pretty expr

instance Pretty Expr where
  pretty = para go
    where
      wrapComplex (x, px)
        | isComplexExpr x = parens px
        | otherwise = px
      go :: ExprF (Expr, Doc ann) -> Doc ann
      go (Lit l) = pretty l
      go (BinOp op p1 p2) = wrapComplex p1 <+> pretty op <+> wrapComplex p2
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
      go (Print (_, e)) = "println(" <> e <> ")"

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
  | TyString
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
  zipMatch TyString TyString = Just TyString
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
      go TyString = "String"
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

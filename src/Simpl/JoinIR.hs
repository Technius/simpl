{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- Vinyl stuff
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Simpl.JoinIR
Description : AST for the JoinIR

Defines the abstract syntax tree for JoinIR, an IR for SimPL based on the IR
presented in /Compiling without Continuations/ by Luke Maurer, Zena Ariola, Paul
Downen, and Simon Peyton Jones (PLDI '17).
-}
module Simpl.JoinIR where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty, pretty, (<>), (<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import Simpl.Ast (BinaryOp(..), Numeric(..), Literal(..), Type, TypeF(..))
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Foldable
import qualified Data.Vinyl as V
import qualified Data.Vinyl.TypeLevel as V
import Data.Singletons.TH (genSingletons)

type Name = Text

type Label = Text

-- | An operation applied to some arguments
data Callable
  = CFunc !Name -- ^ Function
  | CBinOp !BinaryOp -- ^ Binary operator
  | CCast !Numeric -- ^ Numeric cast
  | CCtor !Name -- ^ ADT constructor
  | CPrint -- ^ Print string (temporary)
  | CFunRef !Name -- ^ Static function reference
  deriving (Show)

-- | A value
data JValue
  -- | A variable
  = JVar !Name
  -- | A literal
  | JLit !Literal
  deriving (Show, Eq)

data JBranch a
  = BrAdt Name [Name] !a -- ^ Destructure algebraic data type
  deriving (Functor, Foldable, Traversable, Show)

branchGetExpr :: JBranch a -> a
branchGetExpr = \case
  BrAdt _ _ e -> e

branchGetBindings :: JBranch a -> [Text]
branchGetBindings = \case
  BrAdt _ vars _ -> vars


-- | Represents expressions that must be bound to a join point.
data Joinable a
  -- | If expression on the given variable, with a true branch and a false
  -- branch
  = JIf !JValue !a !a

  -- | Case expression on the given variable
  | JCase !JValue ![JBranch a]
  deriving (Functor, Foldable, Traversable, Show)


-- | The JoinIR expression type. Syntactically, it is in ANF-form with explicit
-- join points.
data JExprF a
  -- | A value
  = JVal !JValue

  -- | A value binding
  | JLet !Name !JValue !a

  -- | A join point. Consists of a label, the variable representing the joined
  -- value, the expression to join, and the next expression.
  | JJoin !Label !Name !(Joinable a) !a

  -- | Jump to the given enclosing join point with the given value.
  | JJump !Label !JValue

  -- | Apply the callable to the arguments, bind the result to the given name,
  -- and continue to the next expression.
  | JApp !Name !Callable ![JValue] !a
  deriving (Functor, Foldable, Traversable, Show)

$(deriveShow1 ''JBranch)
$(deriveShow1 ''Joinable)
$(deriveShow1 ''JExprF)

type JExpr = Fix JExprF

jexprGetVal :: JExprF a -> JValue
jexprGetVal = \case
  JVal v -> v
  JLet n _ _ -> JVar n
  JJoin _ n _ _ -> JVar n
  JJump _ v -> v
  JApp n _ _ _ -> JVar n

instance Pretty Callable where
  pretty = \case
    CFunc name -> pretty name
    CBinOp op -> pretty op
    CCast num -> "cast[" <> pretty num <> "]"
    CCtor name -> pretty name
    CPrint -> "print"
    CFunRef name -> "funref[" <> pretty name <> "]"

instance Pretty JValue where
  pretty = \case
    JVar n -> pretty n
    JLit l -> pretty l

instance Pretty a => Pretty (JBranch a) where
  pretty (BrAdt ctorName varNames expr) =
    PP.hang 2 $ PP.hsep (pretty <$> brPart) <> PP.softline <> pretty expr
    where brPart = [ctorName] ++ varNames ++ ["=>"]

instance Pretty a => Pretty (Joinable a) where
  pretty = \case
    JIf guard trueBr falseBr ->
      PP.hang 2 $ "if" <+> pretty guard <+> PP.group (
        "then" <> PP.softline <> pretty trueBr <> PP.softline
        <> "else" <> PP.softline <> pretty falseBr)
    JCase expr brs ->
      PP.hang 2 $ "case" <+> pretty expr <+> "of" <> PP.hardline <>
      (PP.vsep $ pretty <$> brs)

instance Pretty JExpr where
  pretty = f . unfix
    where
      f :: JExprF JExpr -> PP.Doc ann
      f = \case
        JVal v -> pretty v
        JLet n v next -> PP.hsep ["let", pretty n, "=", pretty v, "in"] <> PP.softline <> pretty next
        JJoin lbl n joinbl next -> PP.align $
          PP.hang 2 (PP.hsep ["join", pretty lbl, "bind", pretty n, "="]
                     <> PP.softline <> pretty joinbl)
          <> PP.hardline <> "in" <+> pretty next
        JJump lbl v -> PP.hsep ["jump", pretty lbl, "with", pretty v]
        JApp name clbl args next ->
          PP.hsep (["let app", pretty name, "=", pretty clbl] ++ (pretty <$> args))
          <> PP.hardline <> pretty next

-- * Annotated [JExpr]s
--
-- Because it's possible to have many different annotations on a single AST, we
-- define a "single" annotated AST that is annotated with an extensible record
-- type at each node. Thus, we can add annotations by extending the record with
-- more fields.

-- | Possible annotations on a [JExpr]
data JFields = ExprType deriving (Show)

genSingletons [ ''JFields ]

-- | Maps each possible annotation label to a type
type family ElF (f :: JFields) :: * where
  ElF 'ExprType = Type

-- | Wrapper for annotation fields
newtype Attr f = Attr { _unAttr :: ElF f }

deriving instance Show (Attr 'ExprType)

-- | Helper function for create annotation fields
(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

-- | Annotations for a typed [JExpr]
type Typed = '[ 'ExprType ]

-- | Creates a type field whose value is the given type
withType :: Type -> Attr 'ExprType
withType ty = SExprType =:: ty

-- | A [JExprF] annotated with some data.
data AnnExprF fields a = AnnExprF { annGetAnn :: V.Rec Attr fields, annGetExpr :: JExprF a }
  deriving (Functor, Foldable, Traversable)

type AnnExpr fields = Fix (AnnExprF fields)

-- | Converts a [JExprF] to an "unannotated" [AnnExprF]
toAnnExprF :: JExprF a -> AnnExprF '[] a
toAnnExprF expr = AnnExprF { annGetAnn = V.RNil, annGetExpr = expr }

-- | Removes all annotations from an [AnnExpr]
unannotate :: AnnExpr fields -> JExpr
unannotate = cata (Fix . annGetExpr)

-- | Adds the given annotation to the expression
addField :: Attr f -> AnnExprF flds a -> AnnExprF (f ': flds) a
addField attr expr = expr { annGetAnn = attr V.:& annGetAnn expr }

-- | Retrieves the type information stored in a typed [AnnExprF]
getType :: V.RElem 'ExprType fields (V.RIndex 'ExprType fields) => AnnExprF fields a -> Type
getType = _unAttr . V.rget @'ExprType . annGetAnn

-- * Misc

-- | Helper for inspecting an [AnnExpr]
newtype PrettyJExprF a = PrettyJExprF (String, JExprF a) deriving (Show)
type PrettyJExpr = Fix PrettyJExprF
$(deriveShow1 ''PrettyJExprF)

-- | Converts an [AnnExpr] into a [PrettyJExpr] so that it can be shown.
prettyAnnExpr :: Show (V.Rec Attr fields) => AnnExpr fields -> PrettyJExpr
prettyAnnExpr = cata $ \expr ->
  Fix (PrettyJExprF (show (annGetAnn expr), annGetExpr expr))

exampleAnnotation :: V.Rec Attr '[ 'ExprType ]
exampleAnnotation = (SExprType =:: Fix (TyNumber NumInt)) V.:& V.RNil

exampleJExpr :: JExpr
exampleJExpr = Fix $
  JJoin "label" "myvar" ifE (Fix (JVal (JVar "myvar")))
  where
    ifE =
      JIf (JLit (LitInt 5))
          (Fix (JJump "label" (JLit (LitInt 10))))
          (Fix (JJump "label" (JLit (LitInt 5))))

exampleTypedJExpr :: AnnExpr '[ 'ExprType ]
exampleTypedJExpr = Fix $ AnnExprF
  { annGetAnn = withType tyInt V.:& V.RNil
  , annGetExpr = JJoin "label" "myvar" ifE varE  }
  where
    tyInt = Fix (TyNumber NumInt)
    varE = Fix $ AnnExprF
      { annGetAnn = withType tyInt V.:& V.RNil
      , annGetExpr = JVal (JVar "myvar") }
    ifE =
      JIf (JLit (LitInt 5)) (jmpE 10) (jmpE 5)
    jmpE v = Fix $ AnnExprF
      { annGetAnn = withType tyInt V.:& V.RNil
      , annGetExpr = JJump "label" (JLit (LitInt v)) }

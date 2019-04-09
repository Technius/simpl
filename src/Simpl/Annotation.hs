{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Simpl.Annotation
Description : Generic AST annotations

To use this module, import it hiding (AnnExprF, AnnExpr), and then import the
desired expression types.
-}
module Simpl.Annotation where

import Data.Functor.Foldable
import qualified Data.Vinyl as V
import qualified Data.Vinyl.TypeLevel as V
import Data.Singletons.TH (genSingletons)
import Text.Show.Deriving (deriveShow1)

import Simpl.Ast (Type)

-- * Annotated expressions
--
-- Because it's possible to have many different annotations on a single AST, we
-- define a "single" annotated AST that is annotated with an extensible record
-- type at each node. Thus, we can add annotations by extending the record with
-- more fields.

-- | Possible annotations
data Fields = ExprType deriving (Show)

genSingletons [ ''Fields ]

-- | Maps each possible annotation label to a type
type family ElF (f :: Fields) :: * where
  ElF 'ExprType = Type

-- | Wrapper for annotation fields
newtype Attr f = Attr { _unAttr :: ElF f }

deriving instance Show (Attr 'ExprType)

-- | Helper function for create annotation fields
(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

-- | Creates a type field whose value is the given type
withType :: Type -> Attr 'ExprType
withType ty = SExprType =:: ty

-- | A [JExprF] annotated with some data.
data AnnExprF expr fields a = AnnExprF { annGetAnn :: V.Rec Attr fields, annGetExpr :: expr a }

deriving instance Functor expr => Functor (AnnExprF expr fields)

type AnnExpr expr fields = Fix (AnnExprF expr fields)

-- | Converts an expression to an "unannotated" [AnnExprF]
toAnnExprF :: expr a -> AnnExprF expr '[] a
toAnnExprF expr = AnnExprF { annGetAnn = V.RNil, annGetExpr = expr }

-- | Converts an expression to an "unannotated" [AnnExpr]
toAnnExpr :: Recursive expr => expr -> AnnExpr (Base expr) '[]
toAnnExpr = cata (Fix . toAnnExprF)

-- | Removes all annotations from an [AnnExpr]
unannotate :: Functor expr => AnnExpr expr fields -> Fix expr
unannotate = cata (Fix . annGetExpr)

-- | Adds the given annotation to the expression
addField :: Attr f -> AnnExprF expr flds a -> AnnExprF expr (f ': flds) a
addField attr expr = expr { annGetAnn = attr V.:& annGetAnn expr }

type HasType fields = V.RElem 'ExprType fields (V.RIndex 'ExprType fields)

-- | Retrieves the type information stored in a typed [AnnExprF]
getType :: HasType fields => AnnExprF expr fields a -> Type
getType = _unAttr . V.rget @'ExprType . annGetAnn

-- * Misc

-- | Helper for inspecting an [AnnExpr]
newtype PrettyExprF expr a = PrettyExprF (String, expr a) deriving (Show)
type PrettyExpr expr = Fix (PrettyExprF expr)
$(deriveShow1 ''PrettyExprF)

-- | Converts an [AnnExpr] into a [PrettyJExpr] so that it can be shown.
prettyAnnExpr :: (Functor expr, Show (V.Rec Attr fields))
              => AnnExpr expr fields
              -> PrettyExpr expr
prettyAnnExpr = cata $ \expr ->
  Fix (PrettyExprF (show (annGetAnn expr), annGetExpr expr))

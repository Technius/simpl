{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Simpl.JoinIR
Description : AST for the JoinIR

Defines the abstract syntax tree for JoinIR, an IR for SimPL based on the IR
presented in /Compiling without Continuations/ by Luke Maurer, Zena Ariola, Paul
Downen, and Simon Peyton Jones (PLDI '17).
-}
module Simpl.JoinIR where

import Data.Text (Text)
import Simpl.Ast (BinaryOp(..), Numeric(..), Literal(..))
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Foldable

type Name = Text

type Label = Text

-- | An operation applied to some arguments
data Callable
  = CFunc !Name -- ^ Function
  | CBinOp !BinaryOp -- ^ Binary operator
  | CCast !Numeric -- ^ Numeric cast
  | CCtor !Name -- ^ ADT constructor
  | CPrint !Name -- ^ Print string (temporary)
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
  | JLet !Name !JValue

  -- | A join point. Consists of a label, the variable representing the joined
  -- value, the expression to join, and the next expression.
  | JJoin !Label !Name !(Joinable a) !a

  -- | Jump to the given enclosing join point with the given value.
  | JJump !Label !JValue

  -- | Apply the callable to the arguments, bind the result to the given name,
  -- and continue to the next expression.
  | JApp !Name !Callable ![Name] !a
  deriving (Functor, Foldable, Traversable, Show)

$(deriveShow1 ''JBranch)
$(deriveShow1 ''Joinable)
$(deriveShow1 ''JExprF)

type JExpr = Fix JExprF

exampleJExpr :: JExpr
exampleJExpr = Fix $
  JJoin "label" "myvar" ifE (Fix (JVal (JVar "myvar")))
  where
    ifE =
      JIf (JLit (LitInt 5))
          (Fix (JJump "label" (JLit (LitInt 10))))
          (Fix (JJump "label" (JLit (LitInt 5))))

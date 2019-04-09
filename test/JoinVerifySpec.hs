{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module JoinVerifySpec
  (joinVerifyTests) where

import qualified Data.Set as Set
import Data.Functor.Foldable (Fix(..))
import Test.Tasty
import Test.Tasty.HUnit
import Simpl.Annotation hiding (AnnExpr, AnnExprF)
import Simpl.Ast (Literal(..))
import Simpl.JoinIR.Syntax
import Simpl.JoinIR.Verify

simpleCtx :: VerifyCtx
simpleCtx = emptyCtx { verifyVars = Set.singleton "x" }

joinVerifyTests :: TestTree
joinVerifyTests = testGroup "JoinIR verify tests"
  [ testCase "Valid if-cfe verifies successfully" $
      assertEqual "" (Right ()) (verify emptyCtx (toAnnExpr goodIfExpr))
  , testCase "Invalid if-cfe fails to verify" $
      assertEqual "" (Left $ NoSuchLabel "badlabel") (verify emptyCtx (toAnnExpr badIfExpr))
  , joinBindingTests
  ]

goodIfExpr :: JExpr
goodIfExpr = Fix $
  JJoin "label" "myvar" ifE (Fix (JVal (JVar "myvar")))
  where
    intVal = Fix . JVal . JLit . LitInt
    ifE = Cfe (intVal 5) $
      JIf (Cfe (intVal 10) (JJump "label"))
          (Cfe (intVal 5) (JJump "label"))

badIfExpr :: JExpr
badIfExpr = Fix $
  JJoin "label" "myvar" ifE (Fix (JVal (JVar "myvar")))
  where
    intVal = Fix . JVal . JLit . LitInt
    ifE = Cfe (intVal 5) $
      JIf (Cfe (intVal 10) (JJump "badlabel"))
          (Cfe (intVal 5) (JJump "label"))


joinBindingTests :: TestTree
joinBindingTests = testGroup "JoinIR variable, label binding tests"
  [ testCase "Known variable verifies successfully" $
      assertEqual "" (Right ()) (verify simpleCtx xVal)
  , testCase "Unknown variable fails to verify" $
      assertEqual "" (Left $ NoSuchVar "x") (verify emptyCtx xVal)
  , testCase "Let binding of unbound variable verifies successfully" $
      assertEqual "" (Right ()) (verify emptyCtx letExpr)
  , testCase "Let binding of bound variable fails to verify" $
      assertEqual "" (Left $ VarRedefinition "x") (verify simpleCtx letExpr)
  , testCase "App binding of unbound variable verifies successfully" $
      assertEqual "" (Right ())  (verify emptyCtx appPrintExpr)
  , testCase "App binding of bound variable fails to verify" $
      assertEqual "" (Left $ VarRedefinition "x") (verify simpleCtx appPrintExpr)
  , testCase "Join binding of unbound variable, label verifies successfully" $
      assertEqual "" (Right ())  (verify emptyCtx joinSimpleExpr)
  , testCase "Join binding of bound variable, unbound label fails to verify" $
      assertEqual "" (Left $ VarRedefinition "x") (verify simpleCtx joinSimpleExpr)
  , testCase "Join binding of unbound variable, bound label fails to verify" $
      assertEqual "" (Left $ LabelRedefinition "lbl") $
        verify (emptyCtx { verifyLabels = Set.singleton "lbl" }) joinSimpleExpr
  , testCase "Jump to unknown label fails to verify" $
      assertEqual "" (Left $ NoSuchLabel "badlbl") (verify simpleCtx badJumpExpr)
  ]
  where
    mke = toAnnExpr . Fix
    mkef = Fix . toAnnExprF
    xVal = mke $ JVal (JVar "x")
    yVal = mke $ JVal (JVar "y")
    letExpr = mkef $ JLet "x" (JLit (LitInt 5)) xVal
    appPrintExpr = mkef $ JApp "x" CPrint [JLit (LitString "foo")] xVal
    joinSimpleExpr = mkef $ JJoin "lbl" "x" (Cfe (mke $ JVal (JLit (LitInt 5))) (JJump "lbl")) xVal
    badJumpExpr = mkef $ JJoin "lbl" "y" (Cfe xVal (JJump "badlbl")) yVal

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Simpl.AstToJoinIR
  ( anfTransform
  ) where

import Data.Functor.Foldable (Fix(..), para, unfix)
import Data.Text (Text)

import Simpl.Ast (Type, TypeF(..))
import qualified Simpl.Ast as A
import qualified Simpl.JoinIR as J

makeJexpr :: Type
          -> J.JExprF (J.AnnExpr '[ 'J.ExprType])
          -> J.AnnExpr '[ 'J.ExprType]
makeJexpr ty = Fix . J.addField (J.withType ty) . J.toAnnExprF

astType :: A.AnnExpr Type -> Type
astType = A.annGetAnn . unfix

transformBranch :: Text -- ^ Return label
                -> A.Branch (A.AnnExpr Type) -- ^ Branches
                -> J.JBranch (J.AnnExpr '[ 'J.ExprType])
transformBranch retLabel (A.BrAdt adtName argNames expr) =
  let jexpr = anfTransform expr (makeJexpr (astType expr) . J.JJump retLabel) in
    J.BrAdt adtName argNames jexpr

anfTransform :: A.AnnExpr Type
             -> (J.JValue -> J.AnnExpr '[ 'J.ExprType])
             -> J.AnnExpr '[ 'J.ExprType]
anfTransform (Fix (A.AnnExprF ty exprf)) cont = case exprf of
  A.Lit lit -> cont (J.JLit lit)
  A.Var name -> cont (J.JVar name)
  A.Let name bindExpr next ->
    anfTransform bindExpr $ \bindVal ->
      makeJexpr (A.annGetAnn (unfix bindExpr)) $
        J.JLet name bindVal (anfTransform next cont)
  A.BinOp op left right ->
    anfTransform left $ \jleft ->
      anfTransform right $ \jright ->
        let name = "TODO" in
        makeJexpr ty (J.JApp name (J.CBinOp op) [jleft, jright] (cont (J.JVar name)))
  A.If guard trueBr falseBr ->
    anfTransform guard $ \jguard ->
      let lbl = "TODO"
          name = "TODO" in
      makeJexpr ty $
        let trueBr' = anfTransform trueBr (makeJexpr (astType trueBr) . J.JJump lbl)
            falseBr' = anfTransform falseBr (makeJexpr (astType falseBr) . J.JJump lbl) in
        J.JJoin lbl name (J.JIf jguard trueBr' falseBr') (cont (J.JVar name))
  A.Case branches expr ->
    anfTransform expr $ \jexpr ->
      let lbl = "TODO"
          name = "TODO"
          jbranches = [transformBranch lbl b | b <- branches] in
      makeJexpr ty (J.JJoin lbl name (J.JCase jexpr jbranches) (cont (J.JVar name)))
  A.Cons ctorName args ->
    let varName = "TODO" in
    collectArgs args [] $ \argVals ->
      makeJexpr ty $
          J.JApp varName (J.CCtor ctorName) argVals (cont (J.JVar varName))
  A.App funcName args ->
    let varName = "TODO" in
    collectArgs args [] $ \argVals ->
      makeJexpr ty $
        J.JApp varName (J.CFunc funcName) argVals (cont (J.JVar varName))
  A.Cast expr numTy ->
    let varName = "TODO" in
      anfTransform expr $ \jexpr ->
        makeJexpr ty $
          J.JApp varName (J.CCast numTy) [jexpr] (cont (J.JVar varName))
  A.Print expr ->
    let varName = "TODO" in
      anfTransform expr $ \jexpr ->
        makeJexpr ty $
          J.JApp varName J.CPrint [jexpr] (cont (J.JVar varName))
  A.FunRef name -> _

-- | Normalize each expression in sequential order, and then run the
-- continuation with the expression values.
collectArgs :: [A.AnnExpr Type]
            -> [J.JValue]
            -> ([J.JValue] -> J.AnnExpr '[ 'J.ExprType])
            -> J.AnnExpr '[ 'J.ExprType]
collectArgs [] vals mcont = mcont (reverse vals)
collectArgs (x:xs) vals mcont = anfTransform x $ \v -> collectArgs xs (v:vals) mcont

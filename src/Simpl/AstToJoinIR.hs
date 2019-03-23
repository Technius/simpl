{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Simpl.AstToJoinIR
  ( anfTransform
  ) where

import Data.Functor.Foldable (Fix(..), para, unfix)

import Simpl.Ast (Type, TypeF(..))
import qualified Simpl.Ast as A
import qualified Simpl.JoinIR as J

makeJexpr :: Type
          -> J.JExprF (J.AnnExpr '[ 'J.ExprType])
          -> J.AnnExpr '[ 'J.ExprType]
makeJexpr ty = Fix . J.addField (J.withType ty) . J.toAnnExprF

astType :: A.AnnExpr Type -> Type
astType = A.annGetAnn . unfix

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
  A.Cons ctorName args ->
    let argVals = [] -- TODO
        varName = "TODO" in
      makeJexpr ty $
          J.JApp varName (J.CCtor ctorName) argVals (cont (J.JVar varName))

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Simpl.Typing where

import Control.Applicative (liftA2)
import Control.Monad (when, foldM, forM, zipWithM)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks, local)
import Control.Monad.Except (ExceptT, MonadError, lift, runExceptT, throwError)
import Control.Unification
import Control.Unification.IntVar
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Functor.Foldable (Fix(..), unfix, cata)
import Data.Text (Text)
import qualified Data.Map as Map

import Simpl.Ast
import Simpl.Analysis

type UVar = IntVar
type UType = UTerm TypeF UVar

type TCExpr = AnnExpr UType
type TypedExpr = AnnExpr Type

data TypeError
  = TyErrOccurs UVar UType
  | TyErrMismatch (TypeF UType) (TypeF UType) -- ^ Expected, actual
  | TyErrArgCount Int Int [Type] -- ^ Expected count, actual count, expected arg types
  | TyErrNoSuchCtor Text
  | TyErrAmbiguousType UType
  | TyErrNoSuchVar Text
  deriving (Show)

instance Fallible TypeF UVar TypeError where
  occursFailure = TyErrOccurs
  mismatchFailure = TyErrMismatch

newtype Typecheck a = Typecheck
  { unTypecheck ::
      ReaderT (SymbolTable Expr) (
        ExceptT TypeError (
          IntBindingT TypeF Identity)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (SymbolTable Expr)
             , MonadError TypeError)

-- | Unify expected with actual type
unifyTy :: UType -> UType -> Typecheck UType
unifyTy t1 t2 = Typecheck . lift $ unify t1 t2

mkMetaVar :: Typecheck UType
mkMetaVar = Typecheck . lift . lift $ UVar <$> freeVar

forceBindings :: UType -> Typecheck UType
forceBindings v = Typecheck . lift $ applyBindings v

literalType :: Literal -> TypeF a
literalType = \case
  LitBool _ -> TyBool
  LitDouble _ -> TyNumber NumDouble
  LitInt _ -> TyNumber NumInt

-- | Annotate every AST node with a unification meta variable
attachExprMetaVar :: Expr -> Typecheck TCExpr
attachExprMetaVar = cataM (\e -> Fix . flip AnnExprF e <$> mkMetaVar)

-- | Resolve all unification variables, returning a well-typed AST
tcExprToTypedExpr :: TCExpr -> Typecheck TypedExpr
tcExprToTypedExpr =
  cataM $ \(AnnExprF uty expr) ->
    utypeToType <$> forceBindings uty >>= \case
      Just ty -> pure . Fix $ AnnExprF ty expr
      Nothing -> throwError $ TyErrAmbiguousType uty

inferType :: Expr -> Typecheck TCExpr
inferType = cata $ \case
  Lit l -> pure $ annotate (Lit l) (UTerm (literalType l))
  BinOp op x y ->
    let tyNumUnknown = UTerm (TyNumber NumUnknown)
        tyBool = UTerm TyBool
        (resTy, unifyResWithArgs) = case op of
          Add -> (tyNumUnknown, True)
          Sub -> (tyNumUnknown, True)
          Mul -> (tyNumUnknown, True)
          Div -> (tyNumUnknown, True)
          Lt -> (tyBool, False)
          Lte -> (tyBool, False)
          Equal -> (tyBool, False)
    in typecheckBinop op resTy unifyResWithArgs x y
  If condM t1M t2M -> do
    cond <- condM
    _ <- unifyTy (extractTy cond) (UTerm TyBool)
    (t1, t2) <- liftA2 (,) t1M t2M
    resTy <- unifyTy (extractTy t1) (extractTy t2)
    pure $ annotate (If cond t1 t2) resTy
  Cons name argsM -> do
    args <- sequence argsM
    let argTys = extractTy <$> args
    ctorRes <- asks (symTabLookupCtor name)
    case ctorRes of
      Just (adtTy, Ctor _ ctorArgTys, _) -> do
        let conArgs = typeToUtype <$> ctorArgTys
        let numConArgs = length conArgs
        when (numConArgs /= length argTys) $
          throwError $ TyErrArgCount numConArgs (length argTys) ctorArgTys
        traverse_ (uncurry unifyTy) (zip conArgs argTys)
        pure $ annotate (Cons name args) (typeToUtype adtTy)
      Nothing -> throwError $ TyErrNoSuchCtor name
  Case branchMs valM -> do
    val <- valM
    let valTy = extractTy val
    branches <- forM branchMs $ \case
      BrAdt ctorName bindings exprM ->
        asks (symTabLookupCtor ctorName) >>= \case
          Just (dataTy, Ctor _ ctorArgs, _) -> do
            when (length bindings /= length ctorArgs) $
              throwError $ TyErrArgCount (length ctorArgs) (length bindings) ctorArgs
            _ <- unifyTy valTy (typeToUtype dataTy)
            let updatedBinds = Map.fromList (bindings `zip` ctorArgs)
            -- Infer result type with ctor args bound
            expr <- local (\t -> t { symTabVars = Map.union (symTabVars t) updatedBinds }) exprM
            pure $ BrAdt ctorName bindings expr
          Nothing -> throwError $ TyErrNoSuchCtor ctorName
    let brTys = extractTy . branchGetExpr <$> branches
    resTy <- mkMetaVar
    annotate (Case branches val) <$> foldM unifyTy resTy brTys
  Let name valM nextM -> do
    val <- valM
    -- TODO: Hack, fix this
    case utypeToType (extractTy val) of
      Just ty -> do
        next <- local (symTabInsertVar name ty) nextM
        pure $ annotate (Let name val next) (extractTy next)
      Nothing -> throwError $ TyErrAmbiguousType (extractTy val)
  Var name ->
    asks (symTabLookupVar name) >>= \case
      Just ty -> pure $ annotate (Var name) (typeToUtype ty)
      Nothing -> throwError $ TyErrNoSuchVar name
  App name args -> do
    -- TODO: Improve error reporting here. If indirect call, then variable type
    -- should be checked first (if invalid, infer function type and reject).
    -- Then check as usual: check parameter count, then check parameter type.
    argsTc <- sequence args
    (params, ty) <- lookupFun name (extractTy <$> argsTc)
    -- Check parameter count
    let numParams = length params
    let paramCount = length params
    when (numParams /= paramCount) $
      throwError $ TyErrArgCount numParams paramCount params
    let unifyExprTy expr pTy =
          annotate (annGetExpr (unfix expr)) <$> unifyTy (extractTy expr) pTy
    -- Check parameter types
    params' <- zipWithM unifyExprTy argsTc (typeToUtype <$> params)
    -- Annotate with result type
    pure $ annotate (App name params') (typeToUtype ty)
  FunRef name ->
    asks (symTabLookupFun name) >>= \case
      Just (params, ty, _) ->
        let paramTys = snd <$> params in
          pure $ annotate (FunRef name) (typeToUtype (Fix $ TyFun paramTys ty))
      Nothing -> throwError $ TyErrNoSuchVar name
  Cast exprM num -> do
    expr <- exprM
    let ty = extractTy expr
    ty' <- unifyTy ty (UTerm (TyNumber NumUnknown))
    let expr' = annotate (annGetExpr . unfix $ expr) ty'
    pure $ annotate (Cast expr' num) (UTerm (TyNumber num))
  where
    annotate :: ExprF TCExpr -> UType -> TCExpr
    annotate expfTc ty = Fix $ AnnExprF ty expfTc

    extractTy :: TCExpr -> UType
    extractTy = annGetAnn . unfix

    typecheckBinop :: BinaryOp -- ^ Operator
                   -> UType -- ^ Result type
                   -> Bool -- ^ Whether result type should be unified with arguments
                   -> Typecheck TCExpr
                   -> Typecheck TCExpr
                   -> Typecheck TCExpr
    typecheckBinop op resultTy unifyArgResult xm ym = do
      (x, y) <- liftA2 (,) xm ym
      xTy <- unifyTy (extractTy x) (UTerm (TyNumber NumUnknown))
      yTy <- unifyTy (extractTy y) xTy
      rTy <- if unifyArgResult then unifyTy yTy resultTy else pure resultTy
      pure $ annotate (BinOp op x y) rTy

    lookupFun :: Text -> [UType] -> Typecheck ([Type], Type)
    lookupFun name argTys =
      asks (symTabLookupVar name) >>= \case
        Just ty ->
          case unfix ty of
            TyFun params resTy -> pure (params, resTy)
            _ -> do
              resTy <- mkMetaVar
              let got = fmap typeToUtype (unfix ty)
                  expected = TyFun argTys resTy
              throwError $ TyErrMismatch expected got
        Nothing ->
          asks (symTabLookupFun name) >>= \case
            Just (params, resTy, _) -> pure (snd <$> params, resTy)
            Nothing -> throwError (TyErrNoSuchVar name)

checkType :: Type -> Expr -> Typecheck TypedExpr
checkType ty expr = do
  typedExpr <- inferType expr
  _ <- unifyTy (annGetAnn . unfix $ typedExpr) (typeToUtype ty)
  tcExprToTypedExpr typedExpr

withExtraVars :: [(Text, Type)] -> Typecheck TypedExpr -> Typecheck TypedExpr
withExtraVars vars action = local (symTabInsertVars vars) action

runTypecheck :: SymbolTable Expr -> Typecheck a -> Either TypeError a
runTypecheck ctx
  = runIdentity
  . evalIntBindingT
  . runExceptT
  . flip runReaderT ctx
  . unTypecheck

utypeToType :: UType -> Maybe Type
utypeToType ut = case ut of
  UTerm t -> Fix <$> traverse utypeToType t
  UVar _ -> Nothing

-- | Direct conversion of a [Type] to a [UType]. Does not instantiate variables.
typeToUtype :: Type -> UType
typeToUtype = cata $ \case
  TyNumber n -> UTerm (TyNumber n)
  TyBool -> UTerm TyBool
  TyAdt n -> UTerm (TyAdt n)
  TyFun args res -> UTerm (TyFun args res)

-- | Get type of an expression, assuming no free variables. Used for debugging.
getTypeOf :: Expr -> Either TypeError Type
getTypeOf expr =
  let table = buildSymbolTable (SourceFile "" [])
      tcAction = inferType expr >>= tcExprToTypedExpr in
  runTypecheck table (annGetAnn . unfix <$> tcAction)

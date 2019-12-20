{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Simpl.Typecheck where

import Control.Applicative (liftA2)
import Control.Monad (when, foldM, forM, zipWithM)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks, local)
import Control.Monad.Except (ExceptT, MonadError, lift, runExceptT, throwError)
import Control.Unification
import Control.Unification.IntVar
import Data.Maybe (fromMaybe)
import Data.Functor.Identity
import Data.Functor.Foldable (Fix(..), unfix, cata)
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vinyl (Rec((:&)))

import Simpl.Ast
import Simpl.SymbolTable
import Simpl.Type
import Simpl.Annotation hiding (AnnExprF, AnnExpr)
import qualified Simpl.Annotation as Ann

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

newtype Typecheck fields a = Typecheck
  { unTypecheck ::
      ReaderT (SymbolTable (AnnExpr fields)) (
        ExceptT TypeError (
          IntBindingT TypeF Identity)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (SymbolTable (AnnExpr fields))
             , MonadError TypeError)

-- | Unify expected with actual type
unifyTy :: UType -> UType -> Typecheck fields UType
unifyTy t1 t2 = Typecheck . lift $ unify t1 t2

mkMetaVar :: Typecheck fields UType
mkMetaVar = Typecheck . lift . lift $ UVar <$> freeVar

forceBindings :: UType -> Typecheck fields UType
forceBindings v = Typecheck . lift $ applyBindings v

literalType :: Literal -> TypeF a
literalType = \case
  LitBool _ -> TyBool
  LitDouble _ -> TyNumber NumDouble
  LitInt _ -> TyNumber NumInt
  LitString _ -> TyString

-- | Annotate every AST node with a unification meta variable
attachExprMetaVar :: AnnExpr fields -> Typecheck fields (AnnExpr ('TCType ': fields))
attachExprMetaVar = cataM (\e -> Fix . flip addField e . withUType <$> mkMetaVar)

-- | Resolve all unification variables, returning a well-typed AST
tcExprToTypedExpr :: AnnExpr ('TCType ': fields)
                  -> Typecheck fields (AnnExpr ('ExprType ': 'TCType ': fields))
tcExprToTypedExpr =
  cataM $ \ae ->
    let uty = getUType ae in
    utypeToType <$> forceBindings uty >>= \case
      Just ty -> pure . Fix $ addField (withType ty) ae
      Nothing -> throwError $ TyErrAmbiguousType uty

inferType :: AnnExpr fields -> Typecheck fields (AnnExpr ('TCType ': fields))
inferType = cata $ \ae -> case annGetExpr ae of
  Lit l -> pure $ annotate (Lit l) (annGetAnn ae) (UTerm (literalType l))
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
    in typecheckBinop op resTy (annGetAnn ae) unifyResWithArgs x y
  If condM t1M t2M -> do
    cond <- condM
    _ <- unifyTy (extractTy cond) (UTerm TyBool)
    (t1, t2) <- liftA2 (,) t1M t2M
    resTy <- unifyTy (extractTy t1) (extractTy t2)
    pure $ annotate (If cond t1 t2) (annGetAnn ae) resTy
  Cons name argsM -> do
    args <- sequence argsM
    let argTys = extractTy <$> args
    ctorRes <- asks (symTabLookupCtor name)
    case ctorRes of
      Just ((adtName, tvars), Ctor _ ctorArgTys, _) -> do
        let numConArgs = length ctorArgTys
        when (numConArgs /= length argTys) $
          throwError $ TyErrArgCount numConArgs (length argTys) ctorArgTys
        -- Instantiate type variables
        substMap <- instantiateVars (Set.fromList tvars)
        let conArgs = substituteUVars substMap . typeToUtype <$> ctorArgTys
        argTys' <- traverse (uncurry unifyTy) (zip conArgs argTys)
        let newTy = UTerm (TyAdt adtName argTys')
        pure $ annotate (Cons name args) (annGetAnn ae) newTy
      Nothing -> throwError $ TyErrNoSuchCtor name
  Case branchMs valM -> do
    val <- valM
    let valTy = extractTy val
    branches <- forM branchMs $ \case
      BrAdt ctorName bindings exprM ->
        asks (symTabLookupCtor ctorName) >>= \case
          Just ((adtName, tvars), Ctor _ ctorArgs, _) -> do
            when (length bindings /= length ctorArgs) $
              throwError $ TyErrArgCount (length ctorArgs) (length bindings) ctorArgs
            -- Instantiate type variables
            substMap <- instantiateVars (Set.fromList tvars)
            let dataTy = Fix (TyAdt adtName (Fix . TyVar <$> tvars))
            _ <- unifyTy valTy (substituteUVars substMap (typeToUtype dataTy))
            let substCtorArgs = substituteUVars substMap . typeToUtype <$> ctorArgs
            -- TODO: Same hack as in let binding
            instCtorArgs <- forM substCtorArgs $ \t -> do
              t' <- utypeToType <$> forceBindings t
              case t' of
                Just t'' -> pure t''
                Nothing -> throwError $ TyErrAmbiguousType t
            let updatedBinds = Map.fromList (bindings `zip` instCtorArgs)
            -- Infer result type with ctor args bound
            expr <- local (\t -> t { symTabVars = Map.union (symTabVars t) updatedBinds }) exprM
            pure $ BrAdt ctorName bindings expr
          Nothing -> throwError $ TyErrNoSuchCtor ctorName
    let brTys = extractTy . branchGetExpr <$> branches
    resTy <- mkMetaVar
    annotate (Case branches val) (annGetAnn ae) <$> foldM unifyTy resTy brTys
  Let name valM nextM -> do
    val <- valM
    valTy <- forceBindings (extractTy val)
    -- TODO: Hack, fix this
    case utypeToType valTy of
      Just ty -> do
        next <- local (symTabInsertVar name ty) nextM
        pure $ annotate (Let name val next) (annGetAnn ae) (extractTy next)
      Nothing -> throwError $ TyErrAmbiguousType (extractTy val)
  Var name ->
    asks (symTabLookupVar name) >>= \case
      Just ty -> pure $ annotate (Var name) (annGetAnn ae) (typeToUtype ty)
      Nothing -> throwError $ TyErrNoSuchVar name
  App name args -> do
    -- TODO: Improve error reporting here. If indirect call, then variable type
    -- should be checked first (if invalid, infer function type and reject).
    -- Then check as usual: check parameter count, then check parameter type.
    argsTc <- sequence args
    (tvars, params, ty) <- lookupFun name (extractTy <$> argsTc)
    -- Check parameter count
    let numParams = length params
    let paramCount = length args
    when (numParams /= paramCount) $
      throwError $ TyErrArgCount numParams paramCount params
    let unifyExprTy expr pTy =
          annotate (annGetExpr (unfix expr)) (annGetAnn ae) <$> unifyTy (extractTy expr) pTy

    -- Instantiate type variables
    substMap <- instantiateVars tvars
    let instParams = substituteUVars substMap . typeToUtype <$> params
    let resTy = substituteUVars substMap (typeToUtype ty)
    -- Check parameter types
    params' <- zipWithM unifyExprTy argsTc instParams
    -- Annotate with result type
    pure $ annotate (App name params') (annGetAnn ae) resTy
  FunRef name ->
    asks (symTabLookupStaticFun name) >>= \case
      Just (tvars, params, ty, _) -> do
        substMap <- instantiateVars tvars
        let paramTys = substituteUVars substMap . typeToUtype . snd <$> params
        let resTy = substituteUVars substMap (typeToUtype ty)
        let funTy = substituteUVars substMap (UTerm (TyFun paramTys resTy))
        pure $ annotate (FunRef name) (annGetAnn ae) funTy
      Nothing -> throwError $ TyErrNoSuchVar name
  Cast exprM num -> do
    expr <- exprM
    let ty = extractTy expr
    ty' <- unifyTy ty (UTerm (TyNumber NumUnknown))
    let expr' = annotate (annGetExpr . unfix $ expr) (annGetAnn ae) ty'
    pure $ annotate (Cast expr' num) (annGetAnn ae) (UTerm (TyNumber num))
  Print exprM -> do
    expr <- exprM
    let ty = extractTy expr
    _ <- unifyTy ty (UTerm TyString)
    pure $ annotate (Print expr) (annGetAnn ae) (UTerm (TyNumber NumInt))
  where
    annotate :: ExprF (AnnExpr ('TCType ': fields)) -> AnnRec fields -> UType -> AnnExpr ('TCType ': fields)
    annotate expfTc fields ty = Fix $ Ann.AnnExprF
      { annGetAnn = withUType ty :& fields
      , annGetExpr = expfTc }

    extractTy :: HasUType fields => AnnExpr fields -> UType
    extractTy = getUType . unfix

    typecheckBinop :: BinaryOp -- ^ Operator
                   -> UType -- ^ Result type
                   -> AnnRec fields -- ^ Annotation fields
                   -> Bool -- ^ Whether result type should be unified with arguments
                   -> Typecheck fields (AnnExpr ('TCType ': fields))
                   -> Typecheck fields (AnnExpr ('TCType ': fields))
                   -> Typecheck fields (AnnExpr ('TCType ': fields))
    typecheckBinop op resultTy annFields unifyArgResult xm ym = do
      (x, y) <- liftA2 (,) xm ym
      xTy <- unifyTy (extractTy x) (UTerm (TyNumber NumUnknown))
      yTy <- unifyTy (extractTy y) xTy
      rTy <- if unifyArgResult then unifyTy yTy resultTy else pure resultTy
      pure $ annotate (BinOp op x y) annFields rTy

    -- | Looks up a function from either variable or function scope. Does not
    -- instantiate type variables (should be handled on a per-construct basis).
    lookupFun :: Text -> [UType] -> Typecheck fields (Set Text, [Type], Type)
    lookupFun name argTys =
      asks (symTabLookupVar name) >>= \case
        Just ty ->
          case unfix ty of
            TyFun params resTy -> pure (Set.empty, params, resTy)
            _ -> do
              resTy <- mkMetaVar
              let got = fmap typeToUtype (unfix ty)
                  expected = TyFun argTys resTy
              throwError $ TyErrMismatch expected got
        Nothing ->
          asks (symTabLookupFun name) >>= \case
            Just (tvars, params, resTy) -> pure (tvars, snd <$> params, resTy)
            Nothing -> throwError (TyErrNoSuchVar name)

checkType :: Type -> AnnExpr fields -> Typecheck fields (AnnExpr ('ExprType ': 'TCType ': fields))
checkType ty expr = do
  typedExpr <- inferType expr
  _ <- unifyTy (getUType . unfix $ typedExpr) (typeToUtype ty)
  tcExprToTypedExpr typedExpr

withExtraVars :: [(Text, Type)] -> Typecheck flds1 (AnnExpr flds2) -> Typecheck flds1 (AnnExpr flds2)
withExtraVars vars = local (symTabInsertVars vars)

runTypecheck :: SymbolTable (AnnExpr fields) -> Typecheck fields a -> Either TypeError a
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
  TyString -> UTerm TyString
  TyAdt n tparams -> UTerm (TyAdt n tparams) -- TODO: Instantiate variables somewhere
  TyFun args res -> UTerm (TyFun args res)
  TyVar n -> UTerm (TyVar n)
  TyBox _ -> error "TyBox should not be in SimPL AST"

-- | Instantiate the type variables with new unification variables
instantiateVars :: Set Text -> Typecheck fields (Map.Map Text UType)
instantiateVars tvars = Map.fromList <$> traverse (\n -> (n,) <$> mkMetaVar) (Set.toList tvars)

-- | Given a substitution, replace any matching TVar terms with their
-- its substituted term.
substituteUVars :: Map.Map Text UType -> UType -> UType
substituteUVars = go
  where
    go substMap = \case
      UTerm (TyVar n) -> fromMaybe (UTerm (TyVar n)) (Map.lookup n substMap)
      UTerm (TyAdt n tparams) -> UTerm (TyAdt n (go substMap <$> tparams))
      UTerm (TyFun args res) -> UTerm (TyFun (go substMap <$> args) (go substMap res))
      t -> t

-- | Get type of an expression, assuming no free variables. Used for debugging.
getTypeOf :: Expr -> Either TypeError Type
getTypeOf expr =
  let table = buildSymbolTable (SourceFile "" [])
      tcAction = inferType (toAnnExpr expr) >>= tcExprToTypedExpr in
  runTypecheck table (getType . unfix <$> tcAction)

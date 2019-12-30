{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Simpl.AstToJoinIR
Description : Provides a function to normalize SimPL AST, transforming it into
              JoinIR.
-}
module Simpl.AstToJoinIR
  ( astToJoinIR
  ) where

import Control.Monad.Reader hiding (guard)
import Data.Functor.Foldable (Fix(..), unfix)
import Data.Functor.Identity
import Data.Text (Text)
import Data.String (fromString)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Simpl.Annotation
import Simpl.SymbolTable
import qualified Simpl.Ast as A
import qualified Simpl.JoinIR.Syntax as J
import Simpl.Type (Type, TypeF(TyBox, TyVar, TyAdt, TyFun), substituteTypeVars, typeRepIsPtr)
import Simpl.Typecheck (literalType)
import Simpl.Util.Supply
import qualified Simpl.Util.Stream as Stream

-- * Public API

astToJoinIR :: HasType flds => SymbolTable (A.AnnExpr flds) -> SymbolTable (J.AnnExpr '[ 'ExprType])
astToJoinIR table = runTransform transformTable (defaultCtx table)

-- * Transformation Monad

data BoxedVal = Boxed | Unboxed deriving (Show, Eq, Ord)

data TransformCtx fields = TransformCtx
  { tcSymTab :: SymbolTable (A.AnnExpr fields)
  , tcJoinLabels :: Set Text
  , tcBoxStatus :: Map Text BoxedVal
  }

defaultCtx :: SymbolTable (A.AnnExpr flds) -> TransformCtx flds
defaultCtx table = TransformCtx
  { tcSymTab = table
  , tcJoinLabels = Set.empty
  , tcBoxStatus = Map.empty }

insertVar :: Text -> Type -> TransformCtx flds -> TransformCtx flds
insertVar name ty ctx = ctx
  { tcSymTab = symTabInsertVar name ty (tcSymTab ctx)
  , tcBoxStatus = Map.insert name (boxedVal ty) (tcBoxStatus ctx)
  }

newtype TransformT fields m a =
  TransformT { unTransform :: ReaderT (TransformCtx fields) (SupplyT Int m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadFreshVar)

deriving instance Monad m => MonadReader (TransformCtx flds) (TransformT flds m)

type Transform fields = TransformT fields Identity

type MonadFreshVar = MonadSupply Int

varSupply :: Stream.Stream Int
varSupply = Stream.iterate (+1) 0

runTransformT :: Monad m => TransformT flds m a -> TransformCtx flds -> m a
runTransformT m table
  = fmap fst
  . flip runSupplyT varSupply
  . flip runReaderT table
  . unTransform
  $ m

runTransform :: Transform flds a -> TransformCtx flds -> a
runTransform m table = runIdentity (runTransformT m table)

-- | Generates a fresh name using the given prefix and lookup function
freshName :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
         => Text -- ^ Prefix
         -> (Text -> TransformCtx flds -> Maybe a) -- ^ Lookup function
         -> m Text
freshName prefix lookupFun = do
  next <- (prefix <>) . fromString . show <$> supply
  asks (lookupFun next) >>= \case
    Nothing -> pure next
    Just _  -> freshName prefix lookupFun

freshVar, freshLabel :: HasType flds => (MonadReader (TransformCtx flds) m, MonadFreshVar m) => m Text
-- | Generate a fresh variable name
freshVar = freshName "var" (\v ctx -> symTabLookupVar v (tcSymTab ctx))
-- | Generate a fresh join label
freshLabel = freshName "join" (\v ctx -> if Set.member v (tcJoinLabels ctx) then Just v else Nothing)

-- * Private utility functions

makeJexpr :: Type
          -> J.JExprF (J.AnnExpr '[ 'ExprType])
          -> J.AnnExpr '[ 'ExprType]
makeJexpr ty = Fix . addField (withType ty) . toAnnExprF

astType :: HasType flds => A.AnnExpr flds -> Type
astType = getType . unfix

getJvalueType :: MonadReader (TransformCtx flds) m => J.JValue -> m Type
getJvalueType = \case
  J.JVar n -> asks (fromJust . symTabLookupVar n . tcSymTab)
  J.JLit l -> pure . Fix $ literalType l

-- | Get boxed type. Left is unboxed, right is boxed.
boxedType :: Type -> Either Type Type
boxedType = \case
  t@(Fix (TyVar _)) -> Right t
  Fix (TyBox t) -> Right t
  t -> Left t

isBoxed :: Type -> Bool
isBoxed t = case boxedType t of { Left _ -> False; Right _ -> True }

boxedVal :: Type -> BoxedVal
boxedVal t = if isBoxed t then Boxed else Unboxed

rebindBoxing :: (HasType flds, MonadFreshVar m, MonadReader (TransformCtx flds) m)
             => J.JValue     -- ^ Variable name
             -> BoxedVal -- ^ Whether to ensure boxed or unboxed
             -> (J.JValue -> m (J.AnnExpr '[ 'ExprType])) -- ^ Continuation
             -> m (J.AnnExpr '[ 'ExprType])
rebindBoxing val b cont = do
  ty <- getJvalueType val
  let create ty' action = do
        name <- case val of { J.JVar n -> pure n; _ -> freshVar }
        makeJexpr ty' . J.JApp name action [val] <$> local (insertVar name ty')(cont (J.JVar name))
  case (boxedType ty, b) of
    (Right ty', Unboxed) -> create ty' J.CUntag
    (Left ty', Boxed) -> create (Fix (TyBox ty')) J.CTag
    _ -> cont val

-- | If needed, rebinds the given value to match the boxing target.
withRebindBoxing :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
                 => BoxedVal -- ^Boxing target
                 -> (J.JValue -> J.JExprF (J.AnnExpr '[ 'ExprType]))
                 -> J.JValue -- ^Expression value
                 -> m (J.AnnExpr '[ 'ExprType])
withRebindBoxing boxVal f val =
  rebindBoxing val boxVal $ \val' -> do
    ty <- getJvalueType val'
    pure . makeJexpr ty . f $ val'

-- * ANF Transformation

-- | Perform ANF transformation on the given symbol table
transformTable :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
               => m (SymbolTable (J.AnnExpr '[ 'ExprType]))
transformTable = do
  table <- asks tcSymTab
  flip symTabTraverseExprs table $ \(tvars, args, ty, expr) ->
    -- Initialize boxing status first, then transform
    let initVars tab = foldl (flip (uncurry insertVar)) tab args
    in (tvars, args, ty, local initVars (transformExpr expr (boxedVal ty)))

-- | Perform ANF transformation on the given expression
transformExpr :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
              => A.AnnExpr flds
              -> BoxedVal -- ^ Expected boxing of the final value
              -> m (J.AnnExpr '[ 'ExprType])
transformExpr expr boxVal = anfTransform expr $ withRebindBoxing boxVal J.JVal

-- | Perform ANF transformation on the branch, afterwards handling control flow.
transformBranch :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
                => J.ControlFlow (J.AnnExpr '[ 'ExprType]) -- ^ Control flow handler
                -> Type -- ^ Type of guard expression
                -> BoxedVal -- ^ Whether branch is expected to be boxed
                -> A.Branch (A.AnnExpr flds) -- ^ Branches
                -> m (J.JBranch (J.AnnExpr '[ 'ExprType]))
transformBranch cf guardTy boxVal (A.BrAdt ctorName argNames expr) = do
  (_, A.Ctor _ argTys, _) <- asks (fromJust . symTabLookupCtor ctorName . tcSymTab)
  argTys' <- case unfix guardTy of
     TyAdt name tvars -> do
       (tvars', _) <- asks (fromJust . symTabLookupAdt name . tcSymTab)
       let tvarMapping = Map.fromList (tvars' `zip` tvars)
       pure $ flip fmap argTys $ \t -> case unfix t of
         TyVar _ ->
           let t' = substituteTypeVars tvarMapping t in
             if t' == t || typeRepIsPtr (unfix t')
             then t' else Fix (TyBox t')
         _ -> substituteTypeVars tvarMapping t
     _ -> pure argTys
  let withScope ctx = foldr (uncurry insertVar) ctx (argNames `zip` argTys')
  jexpr <- local withScope $ anfTransform expr $ withRebindBoxing boxVal J.JVal
  pure $ J.BrAdt ctorName (argNames `zip` argTys') (J.Cfe jexpr cf)


-- | Main ANF transformation logic. Given the SimPL AST, this function will
-- normalize the AST, and then it will feed the final JValue into the given
-- continuation to produce the resulting JoinIR AST.
anfTransform :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
             => A.AnnExpr flds -- ^ Expression to translate
             -> (J.JValue -> m (J.AnnExpr '[ 'ExprType])) -- ^ Continuation
             -> m (J.AnnExpr '[ 'ExprType])
anfTransform (Fix ae) cont = let ty = getType ae in case annGetExpr ae of
  A.Lit lit -> cont (J.JLit lit)
  A.Var name -> cont (J.JVar name)
  A.Let name bindExpr next ->
    anfTransform bindExpr $ \bindVal -> do
      bindTy <- getJvalueType bindVal
      makeJexpr bindTy . J.JLet name bindVal <$>
        local (insertVar name bindTy) (anfTransform next cont)
  A.BinOp op left right ->
    anfTransform left $ \jleft ->
      anfTransform right $ \jright ->
        rebindBoxing jleft Unboxed $ \jl ->
          rebindBoxing jright Unboxed $ \jr -> do
            name <- freshVar
            makeJexpr ty . J.JApp name (J.CBinOp op) [jl, jr] <$>
              local (insertVar name ty) (cont (J.JVar name))
  A.If guard trueBr falseBr ->
    anfTransform guard $ \jguard ->
      -- Guard must be unboxed to compare for truthiness
      rebindBoxing jguard Unboxed $ \jguard' -> do
        guardTy <- getJvalueType jguard'
        let guardCfe = makeJexpr guardTy (J.JVal jguard')
        -- Handle branches
        let transformBr br = anfTransform br $ withRebindBoxing (boxedVal ty) J.JVal
        trueBr'  <- transformBr trueBr
        falseBr' <- transformBr falseBr
        lbl <- freshLabel
        let jmp = J.JJump lbl
        let cfe = J.Cfe guardCfe (J.JIf (J.Cfe trueBr' jmp) (J.Cfe falseBr' jmp))
        -- TODO: Make JJoin node placement more efficient
        name <- freshVar
        makeJexpr ty . J.JJoin lbl name cfe <$>
          local (insertVar name ty) (cont (J.JVar name))
  A.Case branches expr ->
    anfTransform expr $ \jexpr ->
      -- Case value must be unboxed
      rebindBoxing jexpr Unboxed $ \jexpr' -> do
        jTy <- getJvalueType jexpr'
        -- Transform branches
        lbl <- freshLabel
        jbranches <- traverse (transformBranch (J.JJump lbl) jTy (boxedVal ty)) branches
        let jexprCfe = J.Cfe (makeJexpr jTy (J.JVal jexpr')) (J.JCase jbranches)
        name <- freshVar
        -- TODO: Make JJoin node placement more efficient
        makeJexpr ty . J.JJoin lbl name jexprCfe <$>
          local (insertVar name ty) (cont (J.JVar name))
  A.Cons ctorName args ->
    collectArgs args $ \argVals -> do
      (_, A.Ctor _ ctorTyArgs, _) <- asks (fromJust . symTabLookupCtor ctorName . tcSymTab)
      -- Box each argument as needed
      collectRebinds (argVals `zip` (boxedVal <$> ctorTyArgs)) $ \argVals' -> do
        varName <- freshVar
        makeJexpr ty . J.JApp varName (J.CCtor ctorName) argVals' <$>
          local (insertVar varName ty) (cont (J.JVar varName))
  A.App funcName args ->
    collectArgs args $ \argVals -> do
      varName <- freshVar
      -- Function references are stored in variable scope, so we need to perform
      -- lookup there.
      funcCandidate <- asks (fmap unfix . symTabLookupVar funcName . tcSymTab) >>= \case
        Just (TyFun params resTy) -> pure $ Just (params, resTy)
        Just t -> error $ "Variable called as function: " ++ show funcName ++ " : " ++ show t
        Nothing -> asks (fmap (\(_, p, r) -> (snd <$> p, r)) . symTabLookupFun funcName . tcSymTab)
      let (funcArgs, funcRetTy) = fromJust funcCandidate
      let valueBoxPairs = [(val, boxedVal fTy) | (val, fTy) <- argVals `zip` funcArgs]
      collectRebinds valueBoxPairs $ \argVals' -> do
        let ty' = if isBoxed funcRetTy then Fix (TyBox ty) else ty
        makeJexpr ty' . J.JApp varName (J.CFunc funcName) argVals' <$>
          local (insertVar varName ty') (cont (J.JVar varName))
  A.Cast expr numTy ->
    anfTransform expr $ \jexpr ->
      -- Resulting value is unboxed, so use original type
      rebindBoxing jexpr Unboxed $ \jexpr' -> do
        varName <- freshVar
        makeJexpr ty . J.JApp varName (J.CCast numTy) [jexpr'] <$>
          local (insertVar varName ty) (cont (J.JVar varName))
  A.Print expr ->
    anfTransform expr $ \jval ->
      -- Resulting value is unboxed, so use original type
      rebindBoxing jval Unboxed $ \jval' -> do
        varName <- freshVar
        makeJexpr ty . J.JApp varName J.CPrint [jval'] <$>
          local (insertVar varName ty) (cont (J.JVar varName))
  A.FunRef name -> do
    varName <- freshVar
    makeJexpr ty . J.JApp varName (J.CFunRef name) [] <$>
      local (insertVar varName ty) (cont (J.JVar varName))

-- | Utility function for collecting arguments to CPS style functions
collectConts :: (a -> (b -> m c) -> m c)
             -> [a]
             -> ([b] -> m c)
             -> m c
collectConts f as cont = go [] as
  where
    go vals [] = cont (reverse vals)
    go vals (x:xs) = f x $ \v -> go (v:vals) xs

-- | Normalize each expression in sequential order, and then run the
-- continuation with the expression values.
collectArgs :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
            => [A.AnnExpr flds] -- ^ Argument expressions
            -> ([J.JValue] -> m (J.AnnExpr '[ 'ExprType])) -- ^ Continuation
            -> m (J.AnnExpr '[ 'ExprType])
collectArgs = collectConts anfTransform
-- collectArgs = go []
--   where
--     go vals [] mcont = mcont (reverse vals)
--     go vals (x:xs) mcont = anfTransform x $ \v -> go (v:vals) xs mcont

collectRebinds :: (HasType flds, MonadReader (TransformCtx flds) m, MonadFreshVar m)
            => [(J.JValue, BoxedVal)] -- ^ Value-boxing pairs
            -> ([J.JValue] -> m (J.AnnExpr '[ 'ExprType])) -- ^ Continuation
            -> m (J.AnnExpr '[ 'ExprType])
collectRebinds = collectConts (uncurry rebindBoxing)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Simpl.JoinIR.Verify
Description : Verifies validity of a JoinIR AST
-}
module Simpl.JoinIR.Verify
  (verify, VerifyCtx(..), emptyCtx) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Functor.Foldable (cata)
import Data.Functor.Identity
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set

import Simpl.JoinIR

-- * Verification Monad

data VerifyCtx = VerifyCtx
  { verifyVars :: Set Text
  , verifyLabels :: Set Text
  } deriving (Eq, Show)

emptyCtx :: VerifyCtx
emptyCtx = VerifyCtx
  { verifyVars = Set.empty
  , verifyLabels = Set.empty }

ctxWithVar :: Text -> VerifyCtx -> VerifyCtx
ctxWithVar name ctx = ctx { verifyVars = Set.insert name (verifyVars ctx) }

ctxWithLabel :: Text -> VerifyCtx -> VerifyCtx
ctxWithLabel lbl ctx = ctx { verifyLabels = Set.insert lbl (verifyLabels ctx) }

newtype VerifyT m a =
  VerifyT { unVerify :: ReaderT VerifyCtx (ExceptT VerifyError m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader VerifyCtx
           , MonadError VerifyError)

type Verify = VerifyT Identity

runVerifyT :: VerifyT m a -> VerifyCtx -> m (Either VerifyError a)
runVerifyT m ctx
  = runExceptT
  . flip runReaderT ctx
  . unVerify
  $ m

runVerify :: Verify a -> VerifyCtx -> Either VerifyError a
runVerify m ctx = runIdentity (runVerifyT m ctx)

-- * Verification errors

data VerifyError = VarRedefinition Text
                 | LabelRedefinition Text
                 | NoSuchLabel Text
                 | NoSuchVar Text
                 deriving (Show, Eq)

verify :: AnnExpr a -> Either VerifyError ()
verify expr = runVerify (doVerifyExpr expr) emptyCtx

-- | Throw an error if the variable is already bound
checkUnboundVar :: (MonadError VerifyError m, MonadReader VerifyCtx m)
                => Text -> m ()
checkUnboundVar var =
  asks (Set.member var . verifyVars) >>= \case
    True -> throwError $ VarRedefinition var
    False -> pure ()

doVerifyValue :: (MonadError VerifyError m, MonadReader VerifyCtx m)
              => JValue
              -> m ()
doVerifyValue = \case
  JVar name -> asks (Set.member name . verifyVars) >>= \case
    True -> pure ()
    False -> throwError $ NoSuchVar name
  JLit _ -> pure ()

doVerifyExpr :: (MonadError VerifyError m, MonadReader VerifyCtx m)
             => AnnExpr a
             -> m ()
doVerifyExpr = cata (go . annGetExpr)
  where
    go :: (MonadError VerifyError m, MonadReader VerifyCtx m) => JExprF (m ()) -> m ()
    go = \case
      JVal v -> doVerifyValue v
      JLet name val nextM -> do
        doVerifyValue val
        checkUnboundVar name
        local (ctxWithVar name) nextM
      JJoin lbl name cfe nextM -> do
        asks (Set.member lbl . verifyLabels) >>= \case
          True -> throwError $ LabelRedefinition lbl
          False -> pure ()
        local (ctxWithLabel lbl) (doVerifyCfe cfe)
        checkUnboundVar name
        local (ctxWithVar name) nextM
      JApp name _ args nextM -> do
        -- Note: we ignore the callable for now
        _ <- traverse doVerifyValue args
        checkUnboundVar name
        local (ctxWithVar name) nextM

doVerifyCfe :: (MonadError VerifyError m, MonadReader VerifyCtx m)
            => Cfe (m ())
            -> m ()
doVerifyCfe (Cfe exprM cf) = do
  exprM
  case cf of
    JIf trueCfe falseCfe -> do
      doVerifyCfe trueCfe
      doVerifyCfe falseCfe
    JCase branches -> traverse doVerifyBranch branches >> pure ()
    JJump lbl ->
      asks (Set.member lbl . verifyLabels) >>= \case
        True -> pure ()
        False -> throwError $ NoSuchLabel lbl

doVerifyBranch :: (MonadError VerifyError m, MonadReader VerifyCtx m)
               => JBranch (m ())
               -> m ()
doVerifyBranch (BrAdt name args cfe) = do
  checkUnboundVar name
  _ <- traverse checkUnboundVar args
  doVerifyCfe cfe

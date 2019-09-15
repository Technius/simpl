{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module      : Simpl.Util.Supply
Description : An implementation of MonadSupply that assumes infinite
              supplies.
-}
module Simpl.Util.Supply
  ( MonadSupply
  , supply, peek
  , SupplyT(..)
  , runSupplyT
  ) where

import Control.Monad.State
import Control.Monad.Reader (ReaderT)
import Control.Monad.Fail

import Simpl.Util.Stream (Stream)
import qualified Simpl.Util.Stream as Stream

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  peek :: m s

newtype SupplyT s m a = SupplyT { unsupply :: StateT (Stream s) m a }
  deriving (Functor, Applicative, Monad, MonadFail)

instance Monad m => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do
    stream <- get
    put (Stream.tail stream)
    pure (Stream.head stream)
  peek = SupplyT $ gets Stream.head

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply
  peek = lift peek

runSupplyT :: SupplyT s m a -> (Stream s) -> m (a, Stream s)
runSupplyT = runStateT . unsupply

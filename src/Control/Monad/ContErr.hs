{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.ContErr
  ( ContErrT
  , runContErrT
  , contErrT
  ) where

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans

newtype ContErrT e r m a = ContErrT { unContErrT :: ContT r (ReaderT (e -> m r) m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ContErrT e r) where
  -- lift :: m a -> ContErrT e r m a
  lift m = ContErrT $ lift $ lift $ m

contErrT :: (Monad m) => ((a -> m r) -> (e -> m r) -> m r) -> ContErrT e r m a
contErrT f = ContErrT $ ContT $ \cont -> do
  exit <- ask
  let cont' x = runReaderT (cont x) exit
  lift (f cont' exit)

runContErrT :: (Monad m) => ContErrT e r m a -> (a -> m r) -> (e -> m r) -> m r
runContErrT m onResult onExit = runReaderT (runContT (unContErrT m) (lift . onResult)) onExit

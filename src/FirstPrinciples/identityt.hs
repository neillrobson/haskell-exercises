{-# LANGUAGE InstanceSigs #-}

module IdentityT where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import IdComp

newtype IdentityT m a = IdentityT {runIdentityT :: m (Identity a)}

instance (Functor m) => Functor (IdentityT m) where
  fmap :: (Functor m) => (a -> b) -> IdentityT m a -> IdentityT m b
  fmap f = IdentityT . (fmap . fmap) f . runIdentityT

instance (Applicative m) => Applicative (IdentityT m) where
  pure :: (Applicative m) => a -> IdentityT m a
  pure = IdentityT . pure . pure

  (<*>) :: (Applicative m) => IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
  (<*>) (IdentityT miab) = IdentityT . (<*>) ((<*>) <$> miab) . runIdentityT

instance (Monad m) => Monad (IdentityT m) where
  return :: (Monad m) => a -> IdentityT m a
  return = pure

  (>>=) :: (Monad m) => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT mia) >>= f = IdentityT $ do
    (Identity a) <- mia
    runIdentityT $ f a

instance MonadTrans IdentityT where
  lift :: (Monad m) => m a -> IdentityT m a
  lift = IdentityT . fmap Identity

instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO :: (MonadIO m) => IO a -> IdentityT m a
  liftIO = lift . liftIO

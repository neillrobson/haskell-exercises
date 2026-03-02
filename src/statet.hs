{-# LANGUAGE InstanceSigs #-}

import Data.Bifunctor (Bifunctor (first))

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap :: (Functor m) => (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ fmap (first f) . sma

instance (Monad m) => Applicative (StateT s m) where
  pure :: (Monad m) => a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: (Monad m) => StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
    (ab, s') <- smab s
    (a, s'') <- sma s'
    return (ab a, s'')

instance (Monad m) => Monad (StateT s m) where
  return :: (Monad m) => a -> StateT s m a
  return = pure

  (>>=) :: (Monad m) => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    (runStateT . f) a s'

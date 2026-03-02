{-# LANGUAGE InstanceSigs #-}

import Data.Bifunctor (Bifunctor (first))

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap :: (Functor m) => (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ (fmap . fmap) (first f) sma

instance (Monad m) => Applicative (StateT s m) where
  pure :: (Monad m) => a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: (Monad m) => StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
    (ab, s') <- smab s
    (a, s'') <- sma s'
    return (ab a, s'')

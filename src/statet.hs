{-# LANGUAGE InstanceSigs #-}

import Data.Bifunctor (Bifunctor (first))

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap :: (Functor m) => (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ (fmap . fmap) (first f) sma

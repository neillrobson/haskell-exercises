{-# LANGUAGE InstanceSigs #-}

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap :: (Functor m) => (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure :: (Applicative m) => a -> ReaderT r m a
  pure = ReaderT . pure . pure

  (<*>) :: (Applicative m) => ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ do
    mab <- rmab
    ma <- rma
    return $ mab <*> ma

instance (Monad m) => Monad (ReaderT r m) where
  return :: (Monad m) => a -> ReaderT r m a
  return = pure

  (>>=) :: (Monad m) => ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = undefined

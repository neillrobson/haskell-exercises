{-# LANGUAGE InstanceSigs #-}

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
  fmap :: (Functor m) => (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure :: (Applicative m) => a -> MaybeT m a
  pure = MaybeT . pure . pure

  (<*>) :: (Applicative m) => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT mmab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> mmab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return :: (Monad m) => a -> MaybeT m a
  return = pure

  (>>=) :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= f = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just a -> (runMaybeT . f) a

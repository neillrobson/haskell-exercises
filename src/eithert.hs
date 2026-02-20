{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance (Functor m) => Functor (EitherT e m) where
  fmap :: (Functor m) => (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance (Applicative m) => Applicative (EitherT e m) where
  pure :: (Applicative m) => a -> EitherT e m a
  pure = EitherT . pure . pure

  (<*>) :: (Applicative m) => EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT meab) <*> (EitherT mea) = EitherT $ (<*>) <$> meab <*> mea

instance (Monad m) => Monad (EitherT e m) where
  return :: (Monad m) => a -> EitherT e m a
  return = pure

  (>>=) :: (Monad m) => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

eitherT :: (Monad m) => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mab) = do
  ab <- mab
  case ab of
    Left a -> f a
    Right b -> g b

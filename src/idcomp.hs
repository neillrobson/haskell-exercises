{-# LANGUAGE InstanceSigs #-}

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (Identity f) <*> (Identity a) = Identity $ f a

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgab) <*> (Compose fga) = Compose $ ((<*>) . fmap (<*>)) fgab fga

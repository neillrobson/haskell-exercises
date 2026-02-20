{-# LANGUAGE InstanceSigs #-}

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq, Show)

--------------------------------------------------------------------------------

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

--------------------------------------------------------------------------------

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (Identity f) <*> (Identity a) = Identity $ f a

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- (Compose fgab) <*> (Compose fga) = Compose $ ((<*>) . fmap (<*>)) fgab fga
  (Compose fgab) <*> (Compose fga) = Compose $ (<*>) <$> fgab <*> fga

-- An English way to think about the Applicative instance for Compose:
-- We need to get inside the "f" of fgab, to transform `g (a -> b)` to `g a -> g b`.
-- "get inside" is <$>, and the transform is <*>.
-- Once we have `f (g a -> g b)`, we can use <*> again on that and fga (that is, `f (g a)`)
-- to get `f (g b)`.

--------------------------------------------------------------------------------

instance Foldable Identity where
  foldMap :: (Monoid m) => (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Foldable f, Foldable g, Monoid m) => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = foldMap (foldMap f) fga

--------------------------------------------------------------------------------

instance Traversable Identity where
  traverse :: (Applicative f) => (a -> f b) -> Identity a -> f (Identity b)
  traverse f (Identity a) = Identity <$> f a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: (Traversable f, Traversable g, Applicative h) => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

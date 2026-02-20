{- HLINT ignore "Use first" -}
{- HLINT ignore "Use bimap" -}
{-# LANGUAGE InstanceSigs #-}

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap f g (Deux a b) = Deux (f a) (g b)

newtype Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const $ f a

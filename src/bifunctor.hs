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
  bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
  bimap f _ (Const a) = Const $ f a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap :: (a' -> b) -> (c -> d) -> Drei x a' c -> Drei x b d
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap :: (a' -> b) -> (c -> d) -> SuperDrei x a' c -> SuperDrei x b d
  bimap f _ (SuperDrei a b) = SuperDrei a $ f b

newtype SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap :: (a' -> b) -> (c -> d) -> SemiDrei x a' c -> SemiDrei x b d
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap :: (a' -> b') -> (c -> d) -> Quadriceps x y a' c -> Quadriceps x y b' d
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

instance Bifunctor Either where
  bimap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
  bimap f _ (Left a) = Left $ f a
  bimap _ g (Right b) = Right $ g b

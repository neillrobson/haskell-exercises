{-# LANGUAGE InstanceSigs #-}

import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupled :: String -> (String, String)
tupled = do
  capped <- cap
  revved <- rev
  return (capped, revved)

tupled' :: String -> (String, String)
tupled' = (,) <$> cap <*> rev

tupled'' :: String -> (String, String)
tupled'' = cap >>= \c -> rev >>= \r -> return (c, r)

--------------------------------------------------------------------------------

myLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g fa fb = g <$> fa <*> fb

newtype Reader r a = Reader {getReader :: r -> a}

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap g (Reader ra) = Reader $ g . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r

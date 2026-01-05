import Data.List (elemIndex)

added :: Maybe Integer
added = (+ 3) <$> (lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup (2 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex (3 :: Integer) [1 .. 5]

w :: Maybe Int
w = elemIndex (4 :: Integer) [1 .. 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> w

ms :: [Integer]
ms = [1, 2, 3]

ns :: [Integer]
ns = [4, 5, 6]

m :: Maybe Integer
m = lookup 3 $ zip ms ns

n :: Maybe Integer
n = lookup 2 $ zip ms ns

summed :: Maybe Integer
summed = sum <$> ((,) <$> m <*> n)

--------------------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

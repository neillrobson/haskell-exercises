import Data.List (elemIndex)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

--------------------------------------------------------------------------------

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant c) = Constant c

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant c) <*> (Constant c') = Constant $ c <> c'

--------------------------------------------------------------------------------

str :: Maybe String
str = const <$> Just "Hello" <*> pure "World"

tup :: Maybe (Int, Int, String, [Int])
tup = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

--------------------------------------------------------------------------------

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)

import Control.Monad (join)
import Test.QuickCheck (Arbitrary (arbitrary), frequency)
import Test.QuickCheck.Checkers (EqProp ((=-=)), eq, quickBatch)
import Test.QuickCheck.Classes (monad)

-- bind, in terms of fmap and join
bind :: (Monad m) => (a -> m b) -> m a -> m b
bind g = join . (fmap g)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

--------------------------------------------------------------------------------

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap g (Second b) = Second $ g b

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  _ <*> (First a) = First a
  (Second g) <*> (Second b) = Second $ g b

instance Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= g = g b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ First a), (2, return $ Second b)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

checkSum :: IO ()
checkSum = quickBatch $ monad (undefined :: Sum Integer (Char, Char, Char))

--------------------------------------------------------------------------------

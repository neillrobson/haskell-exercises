import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Checkers (EqProp ((=-=)), quickBatch)
import Test.QuickCheck.Classes (applicative)
import Test.QuickCheck.Property (property)

arrPure :: a -> [a]
arrPure = pure

arrApply :: [(a -> b)] -> [a] -> [b]
arrApply = (<*>)

ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

tuplePure :: (Monoid b) => a -> (b, a)
tuplePure = pure

tupleApply :: (Monoid c) => (c, a -> b) -> (c, a) -> (c, b)
tupleApply = (<*>)

funcPure :: (Monoid e) => a -> e -> a
funcPure = pure

funcApply :: (Monoid e) => (e -> a -> b) -> (e -> a) -> (e -> b)
funcApply = (<*>)

--------------------------------------------------------------------------------

data Pair a = Pair a a deriving (Show)

instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair g h) <*> (Pair x y) = Pair (g x) (h y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance (Eq a) => EqProp (Pair a) where
  (Pair v w) =-= (Pair x y) = property $ (v == x) && (w == y)

checkPair :: IO ()
checkPair = quickBatch $ applicative $ Pair ('a', 'b', 'c') ('d', 'e', 'f')

--------------------------------------------------------------------------------

data Two a b = Two a b

data Three a b = Three a b b

data Four a b = Four a a a b

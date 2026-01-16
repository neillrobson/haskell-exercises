import Data.Monoid (Sum)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, sized)
import Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import Test.QuickCheck.Classes (traversable)

-- NB: Think of "Constant" like a 2-tuple but always ignoring the second value.
-- Just like how, for two-tuple's Applicative, the first type needs to be a
-- Monoid (so that they can be squished together),
-- The first type of Constant must also be a Monoid for similar squishing.

-- The Compose datatype squishes two "structure types" into one.
-- Say you have a Maybe [Integer]. That's two structures (Maybe and List)
-- around the Integer data.
-- A regular traverse or bind operation would just lift the outermost structure:
-- in this case, the Maybe.
-- If instead you traversed or bound a Compose Maybe [Integer],
-- the lift would go directly to the Integer data.
-- The abstract type of Compose is "Compose g f a": since classes like Monad
-- and Traversable treat all but the last type parameter as untouchable,
-- g and f (Maybe and []) are both lifted and "a" (Integer) is worked on.

--------------------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap g (Identity x) = Identity $ g x

instance Foldable Identity where
  foldMap g (Identity x) = g x

instance Traversable Identity where
  sequenceA (Identity fa) = Identity <$> fa

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

testIdentity :: IO ()
testIdentity = do
  let trigger :: Identity (Maybe Int, Maybe Int, Int, Sum Int)
      trigger = undefined
  quickBatch $ traversable trigger

--------------------------------------------------------------------------------

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  sequenceA (Constant x) = pure $ Constant x

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

testConstant :: IO ()
testConstant = do
  let trigger :: Constant String (Maybe Int, Maybe Int, Int, Sum Int)
      trigger = undefined
  quickBatch $ traversable trigger

--------------------------------------------------------------------------------

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap g (Yep x) = Yep $ g x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap g (Yep x) = g x

instance Traversable Optional where
  sequenceA Nada = pure Nada
  sequenceA (Yep fx) = Yep <$> fx

--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap g (Cons x xs) = Cons (g x) $ fmap g xs

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap g (Cons x xs) = (g x) <> (foldMap g xs)

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons fx fxs) = Cons <$> fx <*> sequenceA fxs

arbitraryList :: (Arbitrary a) => Int -> Gen (List a)
arbitraryList n
  | n == 0 = return Nil
  | n > 100 = arbitraryList 100
  | otherwise = Cons <$> arbitrary <*> arbitraryList (n - 1)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = sized arbitraryList

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

testList :: IO ()
testList = do
  let trigger :: List (Maybe Int, Maybe Int, Int, Sum Int)
      trigger = undefined
  quickBatch $ traversable trigger

--------------------------------------------------------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap g (Three x y z) = Three x y $ g z

instance Foldable (Three a b) where
  foldMap g (Three _ _ z) = g z

instance Traversable (Three a b) where
  sequenceA (Three a b fz) = Three a b <$> fz

--------------------------------------------------------------------------------

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap g (Bigger a x y z) = Bigger a (g x) (g y) (g z)

instance Foldable (Bigger a) where
  foldMap g (Bigger _ x y z) = (g x) <> (g y) <> (g z)

instance Traversable (Bigger a) where
  sequenceA (Bigger a x y z) = Bigger a <$> x <*> y <*> z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Bigger a x y z

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

testBigger :: IO ()
testBigger = do
  let trigger :: Bigger Char (Maybe Int, Maybe Int, Int, Sum Int)
      trigger = undefined
  quickBatch $ traversable trigger

--------------------------------------------------------------------------------

data S n a = S (n a) a deriving (Eq, Show)

--------------------------------------------------------------------------------

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

import Data.Monoid (Sum)
import Test.QuickCheck (Arbitrary (arbitrary))
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
  (Identity x) =-= (Identity y) = eq x y

testIdentity :: IO ()
testIdentity = do
  let trigger :: Identity ([Int], [Int], Int, Sum Int)
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
  (Constant x) =-= (Constant y) = eq x y

testConstant :: IO ()
testConstant = do
  let trigger :: Constant String ([Int], [Int], Int, Sum Int)
      trigger = undefined
  quickBatch $ traversable trigger

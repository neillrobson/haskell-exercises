import Control.Monad (join)
import Test.QuickCheck (Arbitrary (arbitrary), Testable (property), frequency)
import Test.QuickCheck.Checkers (EqProp ((=-=)), eq, quickBatch)
import Test.QuickCheck.Classes (applicative, functor, monad)

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

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ = const NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  _ =-= _ = property True

checkNope :: IO ()
checkNope = do
  let trigger :: Nope (Char, Int, String)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

--------------------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap g (Identity x) = Identity $ g x

instance Applicative Identity where
  pure = Identity
  (Identity g) <*> (Identity x) = Identity $ g x

instance Monad Identity where
  (Identity x) >>= g = g x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

checkIdentity :: IO ()
checkIdentity = do
  let trigger :: Identity (Int, String, Char)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil xs = xs
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ y Nil = y
fold g y (Cons x xs) = g x $ fold g y xs

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Functor List where
  fmap f (Cons a as) = Cons (f a) $ fmap f as
  fmap _ Nil = Nil

instance Applicative List where
  pure = flip Cons Nil
  fs <*> as = flatMap (flip fmap as) fs

instance Monad List where
  Nil >>= _ = Nil
  x >>= g = flatMap g x

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    frequency
      [ (1, return Nil),
        (1, Cons <$> arbitrary <*> arbitrary)
      ]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

checkList :: IO ()
checkList = do
  let trigger :: List (Int, String, Char)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

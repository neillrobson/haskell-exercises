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

--------------------------------------------------------------------------------

-- Manually re-defining "join"
j :: (Monad m) => m (m a) -> m a
j = (>>= id)

-- Manually re-defining "fmap" (or "liftM")
l1 :: (Monad m) => (a -> b) -> m a -> m b
l1 g = (>>= (return . g))

-- Manually re-defining "liftM2"
l2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
l2 g ma mb = l1 g ma >>= flip l1 mb

-- Manually re-defining "ap"
aa :: (Monad m) => m a -> m (a -> b) -> m b
aa xs fs = do
  f <- fs
  x <- xs
  return $ f x

meh :: (Monad m) => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) g = do
  ys <- meh xs g
  y <- g x
  return $ y : ys

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id

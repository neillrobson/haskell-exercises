import Test.QuickCheck (Testable (property), frequency)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Checkers (EqProp ((=-=)), eq, quickBatch)
import Test.QuickCheck.Classes (applicative)

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

--------------------------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    frequency
      [ (1, return Nil),
        (1, Cons <$> arbitrary <*> arbitrary)
      ]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

--------------------------------------------------------------------------------

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat a
  (ZipList' xs) <*> (ZipList' ys) = ZipList' $ zipWith ($) xs ys

--------------------------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance (Eq a) => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs in take 3000 l
      ys' = let (ZipList' l) = ys in take 3000 l

--------------------------------------------------------------------------------

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap g (Success a) = Success $ g a

instance (Monoid e) => Applicative (Validation e) where
  pure a = Success a
  (Failure x) <*> (Failure y) = Failure $ x <> y
  (Success _) <*> (Failure y) = Failure y
  (Failure x) <*> (Success _) = Failure x
  (Success x) <*> (Success y) = Success $ x y

--------------------------------------------------------------------------------

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency [(1, return $ Failure e), (1, return $ Success a)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (Failure x) =-= (Failure y) = x `eq` y
  (Success x) =-= (Success y) = x `eq` y
  _ =-= _ = property False

--------------------------------------------------------------------------------

main :: IO ()
main = do
  quickBatch $ applicative $ Cons ('a', 'b', 'c') Nil
  quickBatch $ applicative $ ZipList' [('a', 'b', 'c')]
  quickBatch $ applicative $ (Failure "err" :: Validation String (Char, Int, Float))

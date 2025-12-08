{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
import Data.Monoid (Sum)
import Test.QuickCheck (Arbitrary (arbitrary), CoArbitrary, frequency, oneof, quickCheck)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-------------------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-------------------------------------------------------------------

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  sb@(Snd _) <> _ = sb
  _ <> sb@(Snd _) = sb
  a <> _ = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

type OrAssoc = Or Int String -> Or Int String -> Or Int String -> Bool

-------------------------------------------------------------------

newtype Combine a b = Combine {unCombine :: a -> b}

-- Just for the sake of quickcheck property testing
instance Show (Combine a b) where
  show _ = "Combine"

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \a -> f a <> g a

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

combSemigroupAssoc :: (Eq b, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combSemigroupAssoc f g h a = unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

type CombAssoc = Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Int -> Bool

-------------------------------------------------------------------

newtype Comp a = Comp {unComp :: a -> a}

-- Just for the sake of quickcheck property testing
instance Show (Comp a) where
  show _ = "Comp"

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

compSemigroupAssoc :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compSemigroupAssoc f g h a = unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a

type CompAssoc = Comp String -> Comp String -> Comp String -> String -> Bool

-------------------------------------------------------------------

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
  s@(Success _) <> _ = s
  _ <> s@(Success _) = s
  (Failure a) <> (Failure b) = Failure $ a <> b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Failure a), (3, return $ Success b)]

type ValidAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

-------------------------------------------------------------------

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (combSemigroupAssoc :: CombAssoc)
  quickCheck (compSemigroupAssoc :: CompAssoc)
  quickCheck (semigroupAssoc :: ValidAssoc)

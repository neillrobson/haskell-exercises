{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Use tuple-section" #-}
import Data.Monoid (Sum)
import Test.QuickCheck (Arbitrary (arbitrary), CoArbitrary, frequency, oneof, quickCheck)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a <> mempty == a

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type TrivIdent = Trivial -> Bool

-------------------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

type TwoIdent = Two String String -> Bool

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

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ const mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

combSemigroupAssoc :: (Eq b, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combSemigroupAssoc f g h a = unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

combMonoidLeft :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combMonoidLeft f a = unCombine (mempty <> f) a == unCombine f a

combMonoidRight :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combMonoidRight f a = unCombine (f <> mempty) a == unCombine f a

type CombAssoc = Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Int -> Bool

type CombIdent = Combine Int (Sum Int) -> Int -> Bool

-------------------------------------------------------------------

newtype Comp a = Comp {unComp :: a -> a}

-- Just for the sake of quickcheck property testing
instance Show (Comp a) where
  show _ = "Comp"

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

compSemigroupAssoc :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compSemigroupAssoc f g h a = unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a

compMonoidLeft :: (Eq a, Monoid a) => Comp a -> a -> Bool
compMonoidLeft f a = unComp (mempty <> f) a == unComp f a

compMonoidRight :: (Eq a, Monoid a) => Comp a -> a -> Bool
compMonoidRight f a = unComp (f <> mempty) a == unComp f a

type CompAssoc = Comp String -> Comp String -> Comp String -> String -> Bool

type CompIdent = Comp String -> String -> Bool

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

newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance (Semigroup a) => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \s ->
    let (ga, gs) = g s
        (fa, fgs) = f gs
     in (fa <> ga, fgs)

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

f :: Mem Int String
f = Mem $ \s -> ("hi", s + 8)

g :: Mem Int String
g = Mem $ \s -> ("world", div s 2)

h :: Mem Int String
h = Mem $ \s -> ("!!", s - 4)

testMem :: IO ()
testMem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f <> mempty) 0
      rmright = runMem (mempty <> f) 0
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print $ rmleft == runMem f 0
  print $ rmright == runMem f 0
  print $ runMem (f <> g) 0
  print $ runMem (g <> f) 0
  print $ runMem (f <> (g <> h)) 0
  print $ runMem ((f <> g) <> h) 0

-------------------------------------------------------------------

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (combSemigroupAssoc :: CombAssoc)
  quickCheck (compSemigroupAssoc :: CompAssoc)
  quickCheck (semigroupAssoc :: ValidAssoc)
  quickCheck (monoidLeftIdentity :: TrivIdent)
  quickCheck (monoidRightIdentity :: TrivIdent)
  quickCheck (monoidLeftIdentity :: TwoIdent)
  quickCheck (monoidRightIdentity :: TwoIdent)
  quickCheck (combMonoidLeft :: CombIdent)
  quickCheck (combMonoidRight :: CombIdent)
  quickCheck (compMonoidLeft :: CompIdent)
  quickCheck (compMonoidRight :: CompIdent)

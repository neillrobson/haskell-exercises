import Test.QuickCheck (Arbitrary (arbitrary), frequency, quickCheck)

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Optional a) where
  opt <> Nada = opt
  Nada <> opt = opt
  (Only a) <> (Only b) = Only $ a <> b

instance (Monoid a) => Monoid (Optional a) where
  mempty = Nada

--------------------------------------------------------

monoidAssoc :: (Monoid a, Eq a) => a -> a -> a -> Bool
monoidAssoc a b c = (a <> b) <> c == a <> (b <> c)

monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
monoidLeftIdentity a = a == a <> mempty

monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
monoidRightIdentity a = a == mempty <> a

--------------------------------------------------------

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return $ First' Nada), (3, return $ First' $ Only a)]

instance Semigroup (First' a) where
  opt <> First' Nada = opt
  First' Nada <> opt = opt
  a <> _ = a

instance Monoid (First' a) where
  mempty = First' Nada

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

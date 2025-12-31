{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

import Test.QuickCheck (Arbitrary (arbitrary), quickCheck)
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (apply -> f) (apply -> g) x = fmap (g . f) x == (fmap g . fmap f) x

--------------------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

type IdId = Identity Int -> Bool

type IdComp = Fun Int Int -> Fun Int Int -> Identity Int -> Bool

--------------------------------------------------------------------------------

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

type PairId = Pair Int -> Bool

type PairComp = Fun Int Int -> Fun Int Int -> Pair Int -> Bool

--------------------------------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    Two x <$> arbitrary

type TwoId = Two Int String -> Bool

type TwoComp = Fun String Char -> Fun Char Float -> Two Int String -> Bool

--------------------------------------------------------------------------------

check :: IO ()
check = do
  quickCheck (functorIdentity :: IdId)
  quickCheck (functorCompose :: IdComp)
  quickCheck (functorIdentity :: PairId)
  quickCheck (functorCompose :: PairComp)
  quickCheck (functorIdentity :: TwoId)
  quickCheck (functorCompose :: TwoComp)

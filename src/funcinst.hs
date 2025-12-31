{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Test.QuickCheck (Arbitrary (arbitrary), quickCheck)
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (apply -> f) (apply -> g) x = fmap (g . f) x == (fmap g . fmap f) x

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

type IdId = Identity Int -> Bool

type IdComp = Fun Int Int -> Fun Int Int -> Identity Int -> Bool

check :: IO ()
check = do
  quickCheck (functorIdentity :: IdId)
  quickCheck (functorCompose :: IdComp)

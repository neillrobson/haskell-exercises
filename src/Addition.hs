module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: (Integral a) => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n >= d = go (n - d) d (count + 1)
      | otherwise = (count, n)

multiply :: (Eq a, Num a) => a -> a -> a
multiply a b = go a b 0
  where
    go 0 _ t = t
    go _ 0 t = t
    go u v t = go (u - 1) v (t + v)

main :: IO ()
main = hspec $ do
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0 :: Integer)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2 :: Integer)
  describe "Multiplication" $ do
    it "15 times 3 is 45" $ do
      multiply 15 3 `shouldBe` (45 :: Integer)
    it "0 times 3 is 0" $ do
      multiply 0 3 `shouldBe` (0 :: Integer)
    it "15 times 0 is 0" $ do
      multiply 15 0 `shouldBe` (0 :: Integer)
  describe "Addition" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- Using QuickCheck in isolation

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

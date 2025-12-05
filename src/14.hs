import Data.List (sort)
import Test.QuickCheck (quickCheck)

half :: (Fractional a) => a -> a
half x = x / 2

halfIdentity :: (Fractional a) => a -> a
halfIdentity = (* 2) . half

qcHalf :: IO ()
qcHalf = quickCheck (\x -> x == halfIdentity (x :: Double))

--------------------------------------------------------------

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)

listOrderedInt :: [Int] -> Bool
listOrderedInt = listOrdered

qcSort :: IO ()
qcSort = quickCheck $ listOrderedInt . sort

--------------------------------------------------------------

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

plusAInt :: Int -> Int -> Int -> Bool
plusAInt = plusAssociative

plusCInt :: Int -> Int -> Bool
plusCInt = plusCommutative

qcAddition :: IO ()
qcAddition = do
  quickCheck plusAInt
  quickCheck plusCInt

------------------------------------------------------

timesAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
timesAssociative x y z = x * (y * z) == (x * y) * z

timesCommutative :: (Eq a, Num a) => a -> a -> Bool
timesCommutative x y = x * y == y * x

timesAInt :: Int -> Int -> Int -> Bool
timesAInt = timesAssociative

timesCInt :: Int -> Int -> Bool
timesCInt = timesCommutative

qcMultiplication :: IO ()
qcMultiplication = do
  quickCheck timesAInt
  quickCheck timesCInt

import Data.Char (toUpper)
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

--------------------------------------------------------

roundTrip :: (Show a, Read a, Eq a) => a -> Bool
roundTrip x = read (show x) == x

roundTripDouble :: Double -> Bool
roundTripDouble = roundTrip

qcRoundTrip :: IO ()
qcRoundTrip = quickCheck roundTripDouble

--------------------------------------------------------
-- Failures
--------------------------------------------------------

square :: (Num a) => a -> a
square x = x * x

squareIdentity :: (Floating a, Eq a) => a -> Bool
squareIdentity x = x == (square . sqrt) x

sqIdDouble :: Double -> Bool
sqIdDouble = squareIdentity

qcSqId :: IO ()
qcSqId = quickCheck sqIdDouble

--------------------------------------------------------
-- Idempotence
--------------------------------------------------------

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x : xs) = toUpper x : xs

twice :: (a -> a) -> a -> a
twice fn = fn . fn

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

f :: String -> Bool
f x = capitalizeWord x == twice capitalizeWord x && capitalizeWord x == fourTimes capitalizeWord x

f' :: (Ord a) => [a] -> Bool
f' x = sort x == twice sort x && sort x == fourTimes sort x

fpInt :: [Int] -> Bool
fpInt = f'

qcIdempotence :: IO ()
qcIdempotence = do
  quickCheck f
  quickCheck fpInt

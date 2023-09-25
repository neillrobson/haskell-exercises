{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use elem" #-}
import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool a b
  | a < b = a : eftBool (succ a) b
  | a == b = [a]
  | otherwise = []

myWords :: String -> [String]
myWords [] = []
myWords s = takeWhile (/= ' ') s : (myWords . dropWhile (== ' ') . dropWhile (/= ' ')) s

mySqr :: [Integer]
mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube :: [Integer]
myCube = [x ^ 3 | x <- [1 .. 5]]

tuples :: [(Integer, Integer)]
tuples = [(x, y) | x <- mySqr, y <- myCube]

---

justUppers :: String -> String
justUppers = filter isUpper

upperFirst :: String -> String
upperFirst [] = []
upperFirst (x : xs) = toUpper x : xs

upperAll :: String -> String
upperAll = map toUpper

justUpperFirst :: String -> Char
justUpperFirst = toUpper . head

---

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x : xs) = y == x || myElem y xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny y = any (== y)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

myReverseFold :: [a] -> [a]
myReverseFold = foldl (flip (:)) []

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMapDirect :: (a -> [b]) -> [a] -> [b]
squishMapDirect f = squish . map f

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

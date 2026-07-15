{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Maybe (fromJust)

{-# HLINT ignore "Use or" #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Use concatMap" #-}

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

littleWords :: String -> String -> [(Char, Char, Char)]
littleWords s v = [(a, b, c) | a <- s, b <- v, c <- s]

justWithPs :: String -> String -> [(Char, Char, Char)]
justWithPs s = filter (('p' ==) . \(x, _, _) -> x) . littleWords s

seekritFunc :: (Fractional a) => String -> a
seekritFunc x = fromIntegral (sum (map length $ words x)) / fromIntegral (length (words x))

--------------------------------------------------------------

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem1 :: (Eq a) => a -> [a] -> Bool
myElem1 x = foldr ((||) . (== x)) False

myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x -> if f x then (x :) else id) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- The following two definitions avoid using foldr1 or foldl1

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x : xs) =
  let acc = myMaximumBy f xs
   in if f x acc == GT then x else acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy _ [x] = x
myMinimumBy f (x : xs) =
  let acc = myMinimumBy f xs
   in if f x acc == LT then x else acc

-- This one is technically accurate, but the order of execution is off
-- since the head item is evaluated first.

myMaximumFold :: (a -> a -> Ordering) -> [a] -> a
myMaximumFold _ [] = undefined
myMaximumFold f (x : xs) = foldr go x xs
  where
    go n m = if f n m == GT then n else m

-- A sneaky trick: use a Maybe container

myMaximumMaybe :: (a -> a -> Ordering) -> [a] -> a
myMaximumMaybe f =
  fromJust
    . foldr
      ( \x m ->
          case m of
            Nothing -> Just x
            Just y -> if f x y == GT then Just x else Just y
      )
      Nothing

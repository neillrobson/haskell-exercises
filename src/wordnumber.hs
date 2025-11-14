module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> error "argument is not a single digit"

digits :: Int -> [Int]
digits n = go n []
  where
    go i acc
      | i == 0 = acc
      | otherwise = go (div i 10) (mod i 10 : acc)

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

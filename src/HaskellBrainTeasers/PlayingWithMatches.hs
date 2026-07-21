{-# OPTIONS_GHC -Wno-type-defaults #-}

module HaskellBrainTeasers.PlayingWithMatches where

import Data.List (find)

type Triple = (Int, Int, Int)

findTriples :: [Int] -> [Int] -> Maybe Triple -> Maybe Triple
findTriples sides hypotenuses fallback
  | (a : b : _) <- sides, let matches c = a ^ 2 + b ^ 2 == c ^ 2, Just c <- find matches hypotenuses = Just (a, b, c)
  | (_ : rest) <- sides = findTriples rest hypotenuses fallback
  | Just (a, b, c) <- fallback, a ^ 2 + b ^ 2 == c ^ 2 = fallback
  | otherwise = Nothing

main :: IO ()
main = print $ findTriples [1 .. 10] [1 .. 10] (error "boom")

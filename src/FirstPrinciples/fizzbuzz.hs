module FizzBuzz where

import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let x = fizzBuzz n
  put $ x : xs

-- fizzBuzzList :: [Integer] -> [String]
-- fizzBuzzList list = execState (mapM_ addResult list) []

-- silly :: [Integer] -> ()
-- silly list = evalState (mapM_ addResult list) []

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo start end = execState (go start) []
  where
    go n
      | n > end = put []
      | otherwise = (go (n + 1)) >> (addResult n)

-- fizzBuzzCheap :: Integer -> Integer -> [String]
-- fizzBuzzCheap start end = fizzBuzzList [end, end - 1 .. start]

main :: IO ()
main = mapM_ putStrLn $ fizzBuzzFromTo 1 100

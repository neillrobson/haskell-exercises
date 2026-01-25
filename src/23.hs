{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.State (State, evalState, execState, get, put)

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap g (Moi m) = Moi $ \s -> let (a, s') = m s in (g a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi mg) <*> (Moi ma) = Moi $ \s ->
    let (g, s1) = mg s
        (a, s2) = ma s1
     in (g a, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi ma) >>= g = Moi $ \s ->
    let (a, s') = ma s
        (Moi mb) = g a
     in mb s'

--------------------------------------------------------------------------------

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

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

silly :: [Integer] -> ()
silly list = evalState (mapM_ addResult list) []

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo start end = execState (go start) []
  where
    go n
      | n > end = put []
      | otherwise = (go (n + 1)) >> (addResult n)

fizzBuzzCheap :: Integer -> Integer -> [String]
fizzBuzzCheap start end = fizzBuzzList [end, end - 1 .. start]

main :: IO ()
main = mapM_ putStrLn $ fizzBuzzFromTo 1 100

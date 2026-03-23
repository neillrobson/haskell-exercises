module Morra where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State (StateT (runStateT))

data GameState = GameState
  { pScore :: Integer,
    cScore :: Integer
  }
  deriving (Show)

type Game = StateT GameState IO

app :: Game ()
app = do
  lift $ putStrLn "-- P is Player"
  lift $ putStrLn "-- C is Computer"
  lift $ putStrLn "-- Player is odds, Computer is evens."
  lift $ putStr "P: "
  p <- lift getChar
  case p of
    x | x `elem` ['1', '2'] -> lift $ putStrLn "Valid"
    _ -> lift $ putStrLn "Invalid"

main :: IO ()
main = do
  let config = GameState 0 0
  (unit, state) <- runStateT app config
  putStrLn $ "unit: " ++ show unit
  putStrLn $ "state: " ++ show state

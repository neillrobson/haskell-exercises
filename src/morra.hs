module Morra where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State (StateT (runStateT))

data Config = Config
  { pScore :: Integer,
    cScore :: Integer
  }
  deriving (Show)

type Game = StateT Config IO

app :: Game ()
app = do
  lift $ putStrLn "-- P is Player"
  lift $ putStrLn "-- C is Computer"
  lift $ putStrLn "-- Player is odds, Computer is evens."
  lift $ putStr "P: "
  p <- lift getLine
  lift $ putStrLn $ "- P wrote " ++ p

main :: IO ()
main = do
  let config = Config 0 0
  (unit, state) <- runStateT app config
  putStrLn $ "unit: " ++ show unit
  putStrLn $ "state: " ++ show state

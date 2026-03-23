module Morra where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Random (randomRIO)

data GameState = GameState
  { pScore :: Integer,
    cScore :: Integer
  }
  deriving (Show)

type Game = StateT GameState IO

doRound :: Game ()
doRound = do
  lift $ putStr "P: "
  input <- lift getLine
  -- TODO: Input sanitization
  let p = if input == "1" then (1 :: Integer) else 2
  c <- lift $ randomRIO (1, 2)
  lift $ putStrLn $ "C: " ++ show c
  st <- get
  if even (p + c)
    then put $ GameState (pScore st) (1 + cScore st)
    else put $ GameState (1 + pScore st) (cScore st)

main :: IO ()
main = do
  putStrLn "-- P is Player"
  putStrLn "-- C is Computer"
  putStrLn "-- Player is odds, Computer is evens."
  let config = GameState 0 0
  (unit, st) <- runStateT doRound config
  putStrLn $ "unit: " ++ show unit
  putStrLn $ "state: " ++ show st

module Morra where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import System.Random (randomRIO)

data GameState = GameState
  { pScore :: Integer,
    cScore :: Integer
  }
  deriving (Show)

type Game = MaybeT (StateT GameState IO)

doRound :: Game ()
doRound = do
  liftIO $ putStr "P: "
  input <- liftIO getLine
  -- TODO: Input sanitization
  let p = if input == "1" then (1 :: Integer) else 2
  c <- lift $ randomRIO (1, 2)
  liftIO $ putStrLn $ "C: " ++ show c
  st <- lift get
  if even (p + c)
    then do
      liftIO $ putStrLn "- C wins"
      lift . put $ GameState (pScore st) (1 + cScore st)
    else do
      liftIO $ putStrLn "- P wins"
      lift . put $ GameState (1 + pScore st) (cScore st)
  printScore

printScore :: Game ()
printScore = do
  st <- lift get
  liftIO $ putStrLn $ mconcat ["- Player: ", show $ pScore st, " | Computer: ", show $ cScore st]

start :: Game ()
start = forever doRound

main :: IO ()
main = do
  putStrLn "-- P is Player"
  putStrLn "-- C is Computer"
  putStrLn "-- Player is odds, Computer is evens."
  let config = GameState 0 0
  result <- evalStateT (runMaybeT start) config
  print result

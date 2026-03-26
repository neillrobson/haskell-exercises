{-# LANGUAGE ViewPatterns #-}

module Morra where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import qualified Data.Map as M
import System.Random (randomRIO)

type History = M.Map (Integer, Integer) Integer

data GameState = GameState
  { pScore :: Integer,
    cScore :: Integer,
    history :: History,
    moves :: (Integer, Integer, Integer)
  }
  deriving (Show)

type Game = MaybeT (StateT GameState IO)

updateHistory :: Integer -> Game ()
updateHistory i = do
  s <- lift get
  let (b, a, _) = moves s
      hist = history s
      ms' = (i, b, a)
  lift . put $ s {moves = ms'}
  key <- if a == 0 then mzero else return (a, b)
  lift . put $ s {history = M.insert key i hist}

doRound :: Game ()
doRound = do
  liftIO $ putStr "P: "
  input <- liftIO getLine
  p <- do
    case input of
      "1" -> return (1 :: Integer)
      "2" -> return 2
      _ -> mzero
  c <- lift $ randomRIO (1, 2)
  liftIO $ putStrLn $ "C: " ++ show c
  if even (p + c)
    then do
      liftIO $ putStrLn "- C wins"
      lift . modify $ \s@(cScore -> cs) -> s {cScore = succ cs}
    else do
      liftIO $ putStrLn "- P wins"
      lift . modify $ \s@(pScore -> ps) -> s {pScore = succ ps}
  updateHistory p

start :: Game ()
start = forever doRound

main :: IO ()
main = do
  putStrLn "-- P is Player"
  putStrLn "-- C is Computer"
  putStrLn "-- Player is odds, Computer is evens."
  putStrLn "-- (any invalid input immediately ends the game.)"
  let config = GameState 0 0 M.empty (0, 0, 0)
  result <- execStateT (runMaybeT start) config
  let p = pScore result
      c = cScore result
  putStrLn $ mconcat ["- Player: ", show p, " | Computer: ", show c]
  if p == c
    then putStrLn "Tied game!"
    else
      if p < c
        then putStrLn "Computer wins!"
        else putStrLn "Player wins!"

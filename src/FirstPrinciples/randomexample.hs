module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "int out of range: " ++ show x

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

rollThreeTimes :: State StdGen (Die, Die, Die)
rollThreeTimes = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go dsum count gen
      | dsum >= 20 = count
      | otherwise =
          let (roll, nextGen) = randomR (1, 6) gen
           in go (dsum + roll) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go dsum count gen
      | dsum >= n = count
      | otherwise =
          let (roll, nextGen) = randomR (1, 6) gen
           in go (dsum + roll) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 [] g
  where
    go :: Int -> [Die] -> StdGen -> (Int, [Die])
    go dsum dice gen
      | dsum >= n = (length dice, reverse dice)
      | otherwise =
          let (roll, nextGen) = randomR (1, 6) gen
           in go (dsum + roll) ((intToDie roll) : dice) nextGen

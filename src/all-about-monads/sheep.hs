{-# LANGUAGE InstanceSigs #-}

module Sheep where

import Control.Monad (mplus)
import Data.Maybe (maybeToList)

data Sheep = Sheep {name :: String, mother :: Maybe Sheep, father :: Maybe Sheep}

instance Show Sheep where
  show :: Sheep -> String
  show = show . name

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = do
  m <- mother s
  father m

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do
  f <- father s
  gm <- mother f
  mother gm

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do
  g <- mother s
  gf <- father g
  father gf

breedSheep :: Sheep
breedSheep =
  let adam = Sheep "Adam" Nothing Nothing
      eve = Sheep "Eve" Nothing Nothing
      uranus = Sheep "Uranus" Nothing Nothing
      gaea = Sheep "Gaea" Nothing Nothing
      kronos = Sheep "Kronos" (Just gaea) (Just uranus)
      holly = Sheep "Holly" (Just eve) (Just adam)
      roger = Sheep "Roger" (Just eve) (Just kronos)
      molly = Sheep "Molly" (Just holly) (Just roger)
   in Sheep "Dolly" (Just molly) Nothing

--------------------------------------------------------------------------------

mgf :: Sheep -> Maybe Sheep
mgf s = mother s >>= father

fmgm :: Sheep -> Maybe Sheep
fmgm s = father s >>= mother >>= mother

mpgf :: Sheep -> Maybe Sheep
mpgf s = mother s >>= father >>= father

--------------------------------------------------------------------------------

parent :: Sheep -> Maybe Sheep
parent s = mother s `mplus` father s

grandparent :: Sheep -> Maybe Sheep
grandparent s = parent s >>= parent

--------------------------------------------------------------------------------

parents :: Sheep -> [Sheep]
parents s = (maybeToList . mother) s `mplus` (maybeToList . father) s

grandparents :: Sheep -> [Sheep]
grandparents s = parents s >>= parents

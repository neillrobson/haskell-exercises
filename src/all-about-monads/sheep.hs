{-# LANGUAGE InstanceSigs #-}

module Sheep where

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

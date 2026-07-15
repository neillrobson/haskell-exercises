{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Data.Ratio ((%))
import Text.Trifecta

badFraction :: String
badFraction = "1/0"

alsoBad :: String
alsoBad = "10"

shouldWork :: String
shouldWork = "1/2"

shouldAlsoWork :: String
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  return $ numerator % denominator

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return $ numerator % denominator

main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty

  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork

  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

testVirtuous :: IO ()
testVirtuous = do
  let parseFraction' = parseString virtuousFraction mempty

  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork

testSuccessValue :: IO ()
testSuccessValue = do
  let parseWithValue = parseString (integer <* eof) mempty

  print $ parseWithValue "123"
  print $ parseWithValue "123abc"
  print $ parseWithValue "12"
  print $ parseWithValue "ab12"

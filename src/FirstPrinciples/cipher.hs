module Cipher where

import Data.Char (chr, ord)

caesarChrBy :: Int -> Char -> Char
caesarChrBy n = chr . (+ ord 'a') . flip mod 26 . (+ n) . subtract (ord 'a') . ord

caesarChrWith :: Char -> Char -> Char
caesarChrWith = caesarChrBy . subtract (ord 'a') . ord

caesar :: String -> String
caesar = map $ caesarChrBy 5

unCaesar :: String -> String
unCaesar = map $ caesarChrBy (-5)

type Keyword = String

type Message = String

type Cipher = String

vigenere :: Keyword -> Message -> Cipher
vigenere k = zipWith caesarChrWith (cycle k)

main :: IO ()
main = do
  putStr "Enter keyword: "
  kw <- getLine
  putStr "Enter message: "
  msg <- getLine
  putStrLn $ "Cipher: " ++ vigenere kw msg

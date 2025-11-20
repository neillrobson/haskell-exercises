module Cipher where

import Data.Char (chr, ord)

caesarChrBy :: Int -> Char -> Char
caesarChrBy n = chr . (+ ord 'a') . flip mod 26 . (+ n) . subtract (ord 'a') . ord

caesar :: String -> String
caesar = map $ caesarChrBy 5

unCaesar :: String -> String
unCaesar = map $ caesarChrBy (-5)

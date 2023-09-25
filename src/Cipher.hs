module Cipher where

import Data.Char

lowera :: Int
lowera = ord 'a'

mod26 :: Int -> Int
mod26 = (+ lowera) . (`mod` 26) . subtract lowera

caesarChar :: Int -> Char -> Char
caesarChar x = chr . mod26 . (+ x) . ord

caesar :: String -> String
caesar = map (caesarChar 5)

unCaesar :: String -> String
unCaesar = map (caesarChar (-5))

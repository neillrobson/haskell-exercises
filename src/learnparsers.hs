module LearnParsers where

import Control.Applicative ((<|>))
import Text.Trifecta
  ( CharParsing (char, string),
    Parser,
    Parsing (eof, unexpected),
    parseString,
  )

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' <* eof

one' :: Parser a
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' <* eof

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

oneTwoThreeStr :: Parser String
oneTwoThreeStr = string "123" <|> string "12" <|> string "1"

oneTwoThreeStr' :: Parser a
oneTwoThreeStr' = oneTwoThreeStr >> stop

p123 :: String -> IO ()
p123 = print . (parseString oneTwoThreeStr mempty)

myString :: String -> Parser String
myString = traverse char

pNL :: String -> IO ()
pNL s = putStrLn $ '\n' : s

main :: IO ()
main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

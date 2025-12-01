import Data.Maybe (isNothing)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . map (\s -> if isNothing (notThe s) then "a" else s) . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go [] = 0
    go [_] = 0
    go ws@(t : v : _) =
      let n = go $ tail ws
       in if isNothing (notThe t) && head v `elem` "aeiou" then 1 + n else n

countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` "aeiou")

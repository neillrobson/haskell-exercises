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
       in if isNothing (notThe t) && head v `elem` vowels then 1 + n else n

countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` vowels)

newtype Word' = Word' String deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

letterCounts :: String -> (Integer, Integer)
letterCounts = foldr go (0, 0)
  where
    go i (v, c) = if i `elem` vowels then (v + 1, c) else (v, c + 1)

mkWord' :: String -> Maybe Word'
mkWord' s =
  let (v, c) = letterCounts s
   in if v > c then Nothing else Just $ Word' s

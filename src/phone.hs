import Data.Char (toLower)
import Data.List (maximumBy, nub)

-- Valid buttons are 0 through 9, *, and #
type Digit = Char

-- Valid press counts are 1 and up
type Presses = Int

type Entry = [(Digit, Presses)]

type DaPhone = Char -> [Digit]

defaultNumpad :: Char -> Digit
defaultNumpad 'a' = '2'
defaultNumpad 'd' = '3'
defaultNumpad 'g' = '4'
defaultNumpad 'j' = '5'
defaultNumpad 'm' = '6'
defaultNumpad 'p' = '7'
defaultNumpad 't' = '8'
defaultNumpad 'w' = '9'
defaultNumpad _ = undefined

defaultChars :: Digit -> Char
defaultChars '2' = 'c'
defaultChars '3' = 'f'
defaultChars '4' = 'i'
defaultChars '5' = 'l'
defaultChars '6' = 'o'
defaultChars '7' = 's'
defaultChars '8' = 'v'
defaultChars '9' = 'z'
defaultChars '0' = ' '
defaultChars _ = undefined

defaultPhone :: DaPhone
defaultPhone c
  | '.' == c = "#"
  | ',' == c = "##"
  | ' ' == c = "0"
  | '0' == c = c : defaultPhone (defaultChars c)
  | '1' == c = "1"
  | '2' <= c, c <= '9' = c : defaultPhone (defaultChars c)
  | 'A' <= c, c <= 'Z' = '*' : defaultPhone (toLower c)
  | c `elem` "adgjmptw" = [defaultNumpad c]
  | otherwise = case defaultPhone $ pred c of
      dss@(d : _) -> d : dss
      [] -> undefined

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

foldIntoEntry :: Digit -> Entry -> Entry
foldIntoEntry d [] = [(d, 1)]
foldIntoEntry d ess@((e, i) : es)
  | d == e = (e, i + 1) : es
  | otherwise = (d, 1) : ess

reverseTaps :: DaPhone -> Char -> Entry
reverseTaps phone = foldr foldIntoEntry [] . phone

cellPhonesDead :: DaPhone -> String -> Entry
cellPhonesDead phone = concatMap (foldr foldIntoEntry [] . phone)

fingerTaps :: Entry -> Presses
fingerTaps = foldr (\(_, p) ps -> p + ps) 0

frequencies :: (Eq a) => [a] -> [(a, Int)]
frequencies s = nub [(element, count) | element <- s, let count = length $ filter (== element) s]

mostPopular :: (Eq a) => [a] -> a
mostPopular = fst . maximumBy (\(_, a) (_, b) -> compare a b) . frequencies

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular . filter (/= ' ')

mostPopularCost :: [Int]
mostPopularCost = map (fingerTaps . reverseTaps defaultPhone . mostPopularLetter) convo

coolestLetter :: [String] -> Char
coolestLetter = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopular . concatMap words

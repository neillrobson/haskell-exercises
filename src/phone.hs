import Data.Char (toLower)

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

defaultPhone :: DaPhone
defaultPhone c
  | '.' == c = "#"
  | ',' == c = "##"
  | ' ' == c = "0"
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

reverseTaps :: DaPhone -> Char -> Entry
reverseTaps = undefined

cellPhonesDead :: DaPhone -> String -> Entry
cellPhonesDead = undefined

data Mood = Woot | Blah deriving (Show)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

main :: IO ()
main = print $ 3 /= 5

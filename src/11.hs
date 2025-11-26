import Data.Char (toUpper)
import Data.List (intercalate, isPrefixOf, isSuffixOf)

newtype Goats = Goats Int deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany :: Int -> Bool
  tooMany = (> 42)

instance TooMany Goats where
  tooMany :: Goats -> Bool
  tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
  tooMany :: (Int, String) -> Bool
  tooMany (i, _) = i > 43

instance TooMany (Int, Int) where
  tooMany :: (Int, Int) -> Bool
  tooMany (x, y) = x + y > 43

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany :: (Num a, TooMany a) => (a, a) -> Bool
  tooMany (x, y) = tooMany x || tooMany y

---------------------------------------------------

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xss@(x : xs) (y : ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf xss ys

withCapital :: String -> (String, String)
withCapital "" = ("", "")
withCapital w@(c : cs) = (w, toUpper c : cs)

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map withCapital . words

----------------------------------------------------

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x : xs) = toUpper x : xs

capitalizeNext :: String -> String -> String
capitalizeNext word "" = word
capitalizeNext word acc = if "." `isSuffixOf` word then word ++ capitalizeWord acc else word ++ acc

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate ". " . map (capitalizeWord . unwords . words) . wordsWhen (== '.')

splitSentence :: String -> [String]
splitSentence = foldr (\c (a : as) -> if ". " `isPrefixOf` a then [c] : drop 2 a : as else (c : a) : as) [""]

capitalizeParagraph' :: String -> String
capitalizeParagraph' = intercalate ". " . map capitalizeWord . splitSentence

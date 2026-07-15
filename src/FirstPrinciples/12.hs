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

-----------------------------------------------------------------------

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ go i
  where
    go 0 = Zero
    go j = Succ $ go $ j - 1

------------------------------------------------------------------------

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ Nothing = b

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe b Nothing = b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a : _) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : mas) = catMaybes mas
catMaybes (Just a : mas) = a : catMaybes mas

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : _) = Nothing
flipMaybe (Just a : ma_s) = case m_as of
  Nothing -> Nothing
  Just as -> Just (a : as)
  where
    m_as = flipMaybe ma_s

-----------------------------------------------------------

lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where
    go (Left a) as = a : as
    go _ as = as

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where
    go (Right b) bs = b : bs
    go _ bs = bs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where
    go (Left a) (as, bs) = (a : as, bs)
    go (Right b) (as, bs) = (as, b : bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

------------------------------------------------------------

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing -> []
  Just (a, b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))

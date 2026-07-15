import Data.List (sort)

newtype TisAnInteger
  = TisAn Integer

instance Eq TisAnInteger where
  (==) :: TisAnInteger -> TisAnInteger -> Bool
  TisAn i == TisAn i' = i == i'

data TwoIntegers
  = Two Integer Integer

instance Eq TwoIntegers where
  (==) :: TwoIntegers -> TwoIntegers -> Bool
  Two a b == Two a' b' = a == a' && b == b'

data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) :: StringOrInt -> StringOrInt -> Bool
  TisAnInt i == TisAnInt i' = i == i'
  TisAString s == TisAString s' = s == s'
  _ == _ = False

data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
  (==) :: (Eq a) => Pair a -> Pair a -> Bool
  Pair x y == Pair x' y' = x == x' && y == y'

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) :: (Eq a) => Which a -> Which a -> Bool
  ThisOne x == ThisOne x' = x == x'
  ThatOne x == ThatOne x' = x == x'
  _ == _ = False

data Mood = Blah | Woot deriving (Eq, Show)

settleDown x = if x == Woot then Blah else x

type Subject = String

type Verb = String

type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

f :: (RealFrac a) => a
f = 1.0

freud :: Int -> Int
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

mySort :: [Char] -> [Char]
mySort = sort

jung :: [Char] -> Char
jung xs = head $ mySort xs

chk :: (Eq b) => (a -> b) -> a -> b -> Bool
chk f a b = b == f a

arith :: (Num b) => (a -> b) -> Integer -> a -> b
arith f i a = fromInteger i + f a

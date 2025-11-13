data TisAnInteger
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

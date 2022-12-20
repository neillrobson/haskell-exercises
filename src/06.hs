{-# LANGUAGE InstanceSigs #-}
newtype TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) :: TisAnInteger -> TisAnInteger -> Bool
    (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) :: TwoIntegers -> TwoIntegers -> Bool
    (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt =
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) :: StringOrInt -> StringOrInt -> Bool
    (==) (TisAnInt i) (TisAnInt i') = i == i'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _ _ = False

data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
    (==) :: Eq a => Pair a -> Pair a -> Bool
    (==) (Pair n m) (Pair n' m') = n == n' && m == m'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) :: (Eq a, Eq b) => Tuple a b -> Tuple a b -> Bool
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) :: Eq a => Which a -> Which a -> Bool
    (==) (ThisOne a) (ThisOne b) = a == b
    (==) (ThatOne a) (ThatOne b) = a == b
    (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) :: (Eq a, Eq b) => EitherOr a b -> EitherOr a b -> Bool
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    (==) _ _ = False

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

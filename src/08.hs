fact :: Integral a => a -> a
fact n
  | n < 1 = 0
  | n == 1 = 1
  | otherwise = (n *) . fact $ n - 1

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

type Result a = Maybe (a, a)

dividedBy :: Integral a => a -> a -> Result a
dividedBy _ 0 = Nothing
dividedBy 0 _ = Just (0, 0)
dividedBy num den
  | signum num /= signum den = Just $ up num 0
  | otherwise = Just $ down num 0
  where
    down rem count
      | abs rem < abs den = (count, rem)
      | otherwise = down (rem - den) (count + 1)
    up rem count
      | signum rem == signum den = (count, rem)
      | otherwise = up (rem + den) (count - 1)

mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11

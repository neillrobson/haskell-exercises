addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = if odd n then f n else n
    where f = (+ 1)

addFive :: Integer -> Integer -> Integer
addFive x y = (if x > y then y else x) + 5

mflip :: (a -> b -> c) -> b -> a -> c
mflip f x y = f y x

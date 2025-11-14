addOneIfOdd :: (Integral a) => a -> a
addOneIfOdd n = if odd n then f n else n
  where
    f n' = n' + 1

addFive :: (Num a, Ord a) => a -> a -> a
addFive x y = min x y + 5

f1 :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f1 (a, _, c) (d, _, f) = ((a, d), (c, f))

nums :: (Ord a1, Num a1, Num a2) => a1 -> a2
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = flip dodgy 2

-- My own exercise:
-- Create the type declaration and implementation of a function that Shows an arbitrary number of arguments.

class ShowArgsType t where
  catArg :: (Show a) => a -> String -> t

instance ShowArgsType String where
  catArg :: (Show a) => a -> String -> String
  catArg next acc = show next ++ acc

instance (Show a, ShowArgsType r) => ShowArgsType (a -> r) where
  catArg :: (Show i) => i -> String -> a -> r
  catArg next acc a = catArg next $ acc ++ show a

showArgs :: (Show a, ShowArgsType r) => a -> r
showArgs a = catArg a ""

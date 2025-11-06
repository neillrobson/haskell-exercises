functionH :: [a] -> a
functionH (x : _) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = x > y

functionS :: (a, b) -> b
functionS (x, y) = y

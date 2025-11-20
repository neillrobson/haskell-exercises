functionH :: [a] -> a
functionH (x : _) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = x > y

functionS :: (a, b) -> b
functionS (x, y) = y

-----------------------------------------------

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r (_ : xs) = xs

co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f $ g x

------------------------------------------------

data Woot

data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (b, w) = (b, f w)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g i = fst $ g $ f i

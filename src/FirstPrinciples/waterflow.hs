waterflow :: (Num a, Ord a) => [a] -> a
waterflow xs = sum $ zipWith (-) (zipWith min (scanl1 max xs) (scanr1 max xs)) xs

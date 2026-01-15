import Data.Monoid (Any (Any, getAny), Product (Product, getProduct), Sum (Sum, getSum))

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . (foldMap Sum)

product :: (Foldable t, Num a) => t a -> a
product = getProduct . (foldMap Product)

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . (foldMap $ Any . (== x))

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr go Nothing
  where
    go x Nothing = Just x
    go x (Just y) = Just $ min x y

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr go Nothing
  where
    go x Nothing = Just x
    go x (Just y) = Just $ max x y

null :: (Foldable t) => t a -> Bool
null = foldr (const . const True) False

length :: (Foldable t) => t a -> Int
length = foldr (\_ -> (+ 1)) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap g = foldr ((<>) . g) mempty

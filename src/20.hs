import Data.Function (on)
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

--------------------------------------------------------------------------------

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap g (Constant b) = g b

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap g (Two _ b) = g b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap g (Three _ _ c) = g c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap g (Three' _ x y) = on (<>) g x y

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap g (Four' _ x y z) = (g x) <> (g y) <> (g z)

--------------------------------------------------------------------------------

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF g = foldMap go
  where
    go x = if g x then pure x else mempty

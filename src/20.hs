import Data.Monoid (Product (Product, getProduct), Sum (Sum, getSum))

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . (foldMap Sum)

product :: (Foldable t, Num a) => t a -> a
product = getProduct . (foldMap Product)

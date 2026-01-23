module ReaderPractice where

import Control.Applicative
import Data.Maybe

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

-- Same as standard library, just here for practice
-- lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
-- lookup _ [] = Nothing
-- lookup i ((k, v) : kvs)
--   | i == k = Just v
--   | otherwise = ReaderPractice.lookup i kvs

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x y

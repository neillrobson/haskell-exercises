{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use elem" #-}

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem e (x : xs) = e == x || myElem e xs

myElemAny :: (Eq a) => a -> [a] -> Bool
myElemAny e = any (== e)

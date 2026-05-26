{-# LANGUAGE ViewPatterns #-}

module TwentyEight where

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL (x :)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList (unDL -> f) = f []
{-# INLINE toList #-}

infixr 9 `cons`

cons :: a -> DList a -> DList a
cons x xs = DL $ (x :) . unDL xs
{-# INLINE cons #-}

infixl 9 `snoc`

snoc :: DList a -> a -> DList a
snoc xs x = DL $ unDL xs . (x :)
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append (unDL -> f) (unDL -> g) = DL $ f . g
{-# INLINE append #-}

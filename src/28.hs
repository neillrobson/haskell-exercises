module TwentyEight where

import Criterion.Main (bench, defaultMain, whnf)

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL (x :)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList (DL f) = f []
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
append (DL f) (DL g) = DL $ f . g
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    -- Inefficient implementation for demonstration purposes
    go n xs = go (n - 1) (xs ++ [n])

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (xs `snoc` n)

benchmark :: IO ()
benchmark =
  defaultMain
    [bench "concat list" $ whnf schlemiel 123456, bench "concat dlist" $ whnf constructDlist 123456]

--------------------------------------------------------------------------------

-- Goal: dequeue is only ever empty if enqueue is also empty.

data Queue a = Queue {enqueue :: [a], dequeue :: [a]} deriving (Eq, Show)

emptyQ :: Queue a
emptyQ = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue _ []) = Queue [] [x]
push x (Queue e d) = Queue (x : e) d

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue _ []) = Nothing
pop (Queue e [x]) = Just (x, Queue [] (reverse e))
pop (Queue e (x : d)) = Just (x, Queue e d)

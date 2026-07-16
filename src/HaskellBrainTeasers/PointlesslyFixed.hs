module HaskellBrainTeasers.PointlesslyFixed where

import Data.Function (fix)

main :: IO ()
main = print $ go 12
  where
    go = take <*> fix (((:) <*>) . (. succ))

{-
My guess:

doWithNext = (. succ)
prependInput = ((:) <*>)

combo = prependInput . doWithNext

combo says: give me a function that creates a list of values from one value.
I'll call that function with the value's successor, then prepend the original
value to the list.

If combo is re-applied indefinitely to itself, the resulting list will begin
with a long sequence of the input value and each successor. The eventual tail
of the list would be the result of the "base function," but more applications of
combo will increase the length of the prefix.

Since `go` will only take the first n values (first twelve), the result should
be:

[12,13,14,15,16,17,18,19,20,21,22,23]
-}

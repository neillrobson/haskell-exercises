arrPure :: a -> [a]
arrPure = pure

arrApply :: [(a -> b)] -> [a] -> [b]
arrApply = (<*>)

ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

tuplePure :: (Monoid b) => a -> (b, a)
tuplePure = pure

tupleApply :: (Monoid c) => (c, a -> b) -> (c, a) -> (c, b)
tupleApply = (<*>)

funcPure :: (Monoid e) => a -> e -> a
funcPure = pure

funcApply :: (Monoid e) => (e -> a -> b) -> (e -> a) -> (e -> b)
funcApply = (<*>)

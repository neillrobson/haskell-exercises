{-# LANGUAGE InstanceSigs #-}

-- (a -> r) is the "callback" that receives the in-process return value
-- r is the final result type of the whole computation
newtype Cont r a = Cont {runCont :: (a -> r) -> r}

-- Cont r b: "As soon as you tell me what to do with b, I'll make r for you."
-- ret: "This is what you should do with b."
-- c (\a -> ...): "I know what I want to do with 'a' now. Make it and give it to this lambda."
instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont c) = Cont $ \ret ->
    c $ \a -> ret $ f a

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont $ \ret -> ret a

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (Cont rab) <*> (Cont ra) = Cont $ \ret ->
    rab $ \ab ->
      ra $ \a -> ret $ ab a

instance Monad (Cont r) where
  return :: a -> Cont r a
  return = pure

  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (Cont ra) >>= f = Cont $ \ret ->
    -- This is what you should do with "a":
    -- generate a "Cont r b",
    -- then tell it to use "ret" (b to r) in order to make r.
    ra $ \a -> runCont (f a) ret

-- callCC takes a function that returns a Cont.
-- However, that Cont is constructed with an "escape hatch":
-- If the Cont (m b) is ever executed,
-- which requires supplying an value of "a",
-- the execution of the Cont returns early with that supplied value.
class (Monad m) => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (Cont r) where
  callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
  callCC f = Cont $ \ar -> runCont undefined ar

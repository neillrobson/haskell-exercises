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
  pure a = Cont ($ a)

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
  callCC f = Cont $ \ar ->
    runCont (f $ \a -> Cont $ \_ -> ar a) ar

--               |--   This is "k"   --|
--                ^ when given "a", return a continuation that...
--                             ^ ...ignores whatever else, not calling it,
--                                  ^ and produces "r" from "a".

quux :: Cont a Integer
quux = callCC $ \k -> do
  _ <- k 5
  return 25

quux1 :: Cont a Integer
quux1 = Cont $ \ar ->
  runCont ((\k -> do _ <- k 5; return 25) $ \a -> Cont $ \_ -> ar a) ar

quux2 :: Cont a Integer
quux2 = Cont $ \ar ->
  runCont (do _ <- (\a -> Cont $ \_ -> ar a) 5; return 25) ar

quux3 :: Cont a Integer
quux3 = Cont $ \ar ->
  runCont (do _ <- Cont $ \_ -> ar 5; return 25) ar

quux4 :: Cont a Integer
quux4 = Cont $ \ar ->
  runCont (Cont (\_ -> ar 5) >> return 25) ar

quux5 :: Cont a Integer
quux5 = Cont $ \ar ->
  runCont (Cont (\_ -> ar 5) >>= const (return 25)) ar

quux6 :: Cont a Integer
quux6 = Cont $ \ar ->
  runCont (Cont $ \ret -> (\_ -> ar 5) $ \_ -> runCont (return 25) ret) ar

quux7 :: Cont a Integer
quux7 = Cont $ \ar ->
  runCont (Cont $ \_ -> ar 5) ar

quux8 :: Cont a Integer
quux8 = Cont $ \ar ->
  (\_ -> ar 5) ar

quux9 :: Cont a Integer
quux9 = Cont $ \ar -> ar 5

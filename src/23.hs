{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap g (Moi m) = Moi $ \s -> let (a, s') = m s in (g a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi mg) <*> (Moi ma) = Moi $ \s ->
    let (g, s1) = mg s
        (a, s2) = ma s1
     in (g a, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi ma) >>= g = Moi $ \s ->
    let (a, s') = ma s
        (Moi mb) = g a
     in mb s'

data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil as = as
append (Cons a as) as' = Cons a $ append as as'

instance Functor List where
  fmap f (Cons a as) = Cons (f a) $ fmap f as
  fmap _ Nil = Nil

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> as = append (f <$> as) (fs <*> as)

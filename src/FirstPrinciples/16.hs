{-# LANGUAGE FlexibleInstances #-}

data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First $ f a
  fmap _ (Second b) = Second b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something $ f b
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--------------------------------------------------------------------------------

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

newtype K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K $ f b

newtype EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

newtype LiftItOut f a = LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
  fmap fn (LiftItOut fa) = LiftItOut $ fmap fn fa

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap fn (DaWrappa fa ga) = DaWrappa (fmap fn fa) (fmap fn ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap fn (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap fn gb

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap fn (Notorious go ga gt) = Notorious go ga $ fmap fn gt

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str $ f x
  fmap f (Read sa) = Read $ f . sa

module TwentySix where

import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Reader (Reader, ReaderT (runReaderT), runReader)

rtm :: ReaderT r Maybe Integer
rtm = undefined

rtm' :: r -> Maybe Integer
rtm' = runReaderT rtm

mtr :: MaybeT (Reader r) Integer
mtr = undefined

mtr' :: Reader r (Maybe Integer)
mtr' = runMaybeT mtr

mtr'' :: r -> Maybe Integer
mtr'' = runReader mtr'

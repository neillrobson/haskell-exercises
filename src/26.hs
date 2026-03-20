module TwentySix where

import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Reader (Reader, ReaderT (ReaderT, runReaderT), reader, runReader)
import Data.Functor.Identity (Identity (Identity))

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

--------------------------------------------------------------------------------

rDec :: (Num a) => Reader a a
rDec = reader $ subtract 1

rShow :: (Show a) => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \x -> do
  putStrLn $ "Hi: " ++ show x
  return $ x + 1

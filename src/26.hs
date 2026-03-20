module TwentySix where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Reader (Reader, ReaderT (ReaderT, runReaderT), ask, reader, runReader)
import Control.Monad.Trans.State (StateT, get, put)
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
rPrintAndInc = do
  x <- ask
  lift $ putStrLn $ "Hi: " ++ show x
  return $ x + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  x <- get
  lift $ putStrLn $ "Hi: " ++ show x
  put $ x + 1
  return $ show x

--------------------------------------------------------------------------------

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  -- Need lift!
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  -- Need runMaybeT!
  excite <- runMaybeT maybeExcite

  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn $ "Good, was very excite: " ++ e

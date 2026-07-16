{-# OPTIONS_GHC -fno-full-laziness #-}

module HaskellBrainTeasers.UnsafeToTheMax where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

unsafeMax :: [Int] -> IO Int
unsafeMax vals = do
  for_ vals $ \val -> do
    currentMax <- readIORef maxRef
    when (val > currentMax) $
      writeIORef maxRef val
  readIORef maxRef
  where
    maxRef = unsafePerformIO $ newIORef 0

main :: IO ()
main = do
  four <- unsafeMax [4, 3, 2]
  three <- unsafeMax [3, 2, 1]
  two <- unsafeMax [2, 1, 0]
  print [four, three, two]

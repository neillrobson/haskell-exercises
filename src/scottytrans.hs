{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT (..))

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- captureParam "word"
    (ActionT . ReaderT . const) (putStrLn "hello")

    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

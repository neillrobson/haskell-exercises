import Control.Monad
import Data.Char (toLower)
import Distribution.Compat.Prelude (exitSuccess)

justalpha :: String -> String
justalpha = filter (`elem` "abcdefghijklmnopqrstuvwxyz") . map toLower

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let normalized = justalpha line1
  if normalized == reverse normalized
    then putStrLn "It's a palindrome!"
    else do
      putStrLn "Nope!"
      exitSuccess

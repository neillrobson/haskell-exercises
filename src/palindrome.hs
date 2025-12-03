import Control.Monad
import Distribution.Compat.Prelude (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  if line1 == reverse line1
    then putStrLn "It's a palindrome!"
    else do
      putStrLn "Nope!"
      exitSuccess

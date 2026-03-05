import Control.Monad.Trans.Cont

fun :: (Monad m) => p -> m String
fun _ = return "hi"

fun' :: (Monad m) => (String -> m a) -> m String
fun' escape = do
  _ <- escape "ahoy"
  return "die die die"

main :: IO ()
main = print $ runCont (callCC fun') id

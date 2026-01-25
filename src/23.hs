testReader :: Integer -> Integer
testReader = do
  x <- (+ 7)
  y <- (* 2)
  return $ x - y

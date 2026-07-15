module HaskellBrainTeasers.FirstFourDigitPrime where

primes :: [Int]
primes = sieve [2 ..]
  where
    sieve [] = []
    sieve (prime : candidates) = prime : sieve (filter divisible candidates)
      where
        divisible candidate = candidate `mod` prime /= 0

firstBigNumber :: [Int] -> Maybe Int
firstBigNumber = foldr findFirst Nothing
  where
    findFirst someNum isFound
      | length (show someNum) >= 4 = Just someNum
      | otherwise = isFound

main :: IO ()
main = case firstBigNumber primes of
  Nothing -> putStrLn "No big primes"
  Just aPrime -> putStrLn $ "The first big prime is " <> show aPrime

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

littleWords :: String -> String -> [(Char, Char, Char)]
littleWords s v = [(a, b, c) | a <- s, b <- v, c <- s]

justWithPs :: String -> String -> [(Char, Char, Char)]
justWithPs s = filter (('p' ==) . \(x, _, _) -> x) . littleWords s

seekritFunc :: (Fractional a) => String -> a
seekritFunc x = fromIntegral (sum (map length $ words x)) / fromIntegral (length (words x))

import Data.Char

justUpper :: String -> String
justUpper = filter isUpper

upperFirst :: String -> String
upperFirst "" = ""
upperFirst (c : cs) = toUpper c : cs

upperAllRecurse :: String -> String
upperAllRecurse "" = ""
upperAllRecurse (c : cs) = toUpper c : upperAllRecurse cs

justUpperFirst :: String -> Char
justUpperFirst = toUpper . head

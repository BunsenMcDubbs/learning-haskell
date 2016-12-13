import Data.List
import Data.Char

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode offset msg = encode (negate offset) msg

firstTo :: Int -> Maybe Int
firstTo a = find (\x -> a == sum (map digitToInt (show x)))  [1..]

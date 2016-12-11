mergesort :: (Ord a) => [a] -> [a]
mergesort []    = []
mergesort [x]   = [x]
mergesort x     = 
    let halfLength  = fromIntegral (length x) / 2.0
        firstHalf   = mergesort (take (floor halfLength) x)
        secondHalf  = mergesort [ s | s <- x, elem s firstHalf == False ]
    in  merge firstHalf secondHalf

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge xa@(x:xs) ya@(y:ys)
    | x < y = x : merge xs ya
    | otherwise = y : merge xa ys

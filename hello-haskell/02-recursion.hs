-- replicate
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

-- take
take' :: Int -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n-1) xs

-- reverse
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat (replicate infinity)
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- replicate 2.0 (using take and repeat)
replicate'' :: Int -> a -> [a]
replicate'' n x = take' n (repeat' x)

-- zip
zip' :: [a] -> [b] -> [(a,b)]
zip' _ []           = []
zip' [] _           = []
zip' (x:xs) (y:ys)  = (x,y) : zip' xs ys

-- elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs


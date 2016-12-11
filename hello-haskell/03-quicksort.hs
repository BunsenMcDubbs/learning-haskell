quicksort :: (Ord a) => [a] -> [a]
quicksort []        = []
quicksort (x:[])    = [x]
quicksort (x:xs)    =
    let lte = [ lte | lte <- xs, lte <= x ]
        gt  = [ gt  | gt  <- xs, gt  >  x ]
    in (quicksort lte) ++ [x] ++ (quicksort gt)


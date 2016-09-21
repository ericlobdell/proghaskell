

















qsort :: [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

qsortr :: [a] -> [a]
qsortr [] = []
qsortr (x:xs) = qsortr larger ++ [x] ++ qsortr smallers
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]





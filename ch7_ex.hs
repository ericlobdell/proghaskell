-- 1 Show how the list comprehension [  f x | x <- xs, p x ] can be re-expressed
--   using the higher-order functions map and filter

mf f p = map f . filter p

-- 2 Without looking at the definitions from the standard prelude, define the
--   following higher-order library functions on lists

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = False
all' p [x] = p x
all' p (x:xs) = p x && all' p xs

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p [x] = p x
any' p (x:xs) = p x || any' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else x:xs


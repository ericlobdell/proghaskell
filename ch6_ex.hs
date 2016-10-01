-- NOTES

fac :: Int -> Int
fac n = product [1..n]

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac (n-1)

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

-- insertion sort
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs 

-- multiple recursion
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- mutual recursion
even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)

-- exercises
-- 2 Define a recursive function sumdown :: Int -> Int 
--   that returs the sum of non-negative integers from a give 
--   value down to 0

sumdown :: Int -> Int
sumdown n | n <= 0 = 0
          | otherwise = n + sumdown (n - 1)


-- 3 Define the exponention operator (^) for non-negative integers
--   using the same pattern of recursion as the multiplication operator *
--   and show how the the expression 2 ^ 3 is evaluated using your definition

-- (^) :: Int -> Int -> Int
-- _ ^ 0 = 1
-- 0 ^ _ = 0
-- x ^ n = x * ( x ^ (n - 1) )

-- 4 Define a recursive function euclid :: Int -> Int -> Int
--   that implements Euclid's algorithm for calculating the 
--   greatest common devisor of two non-negative integers:
--   If the two numbers are equal, this number is the result
--   otherwise, the smaller number is subtracted from the larger
--   and the same process is repeated

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | otherwise = euclid (larger - smaller) smaller
                where 
                    (larger, smaller) = if x > y then (x,y) else (y,x)








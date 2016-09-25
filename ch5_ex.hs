
-- Notes:

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

len :: [a] -> Int
len xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k' == k]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]


-- Exercises

-- 1. Using a list comprehension, giv an expression that calculates the sum of 1^2, 2^2, 3^2...100^2 it the first 100 integer aquares

sq100 :: Int
sq100 = sum [ x^2 | x <- [1..100] ]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]



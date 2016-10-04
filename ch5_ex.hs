
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

-- 1. Using a list comprehension, give an expression that calculates the sum of 1^2, 2^2, 3^2...100^2 it the first 100 integer aquares

sq100 :: Int
sq100 = sum [ x^2 | x <- [1..100] ]

-- 2 Suppose that a coordinate grid of size m x n is given by the list of all pairs (x,y)
--   of integers such that 0 <= x <= m and 0 <= y <= n. Using a list comprehension, define
--   a function grid :: Int -> Int -> [(Int,Int)] that returns a coordinate grid of a given size. 
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3 Using a list comprehension and the grid function above, define a function 
--   square :: Int -> [(Int,Int)] that returns a coordinate square of size n,
--   excluding the diagonal (0,0) to (n,n).  
square :: Int -> [(Int, Int)]
square n = [ (x,y) | (x,y) <- grid n n, x /= y ]

-- 4 In a similar way top the function length, show how the library function 
--   replicate :: Int -> a -> [a] that produces a list of identical elements
--   can be defined using a list comprehension.
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 5 A triple (x,y,z) of positive integers is Pathagorean if it satisfies the equation
--   x^2 + y^2 = z^2. Using a list comprehension with 3 generators, define a function 
--   pyths :: Int -> [(Int,Int,Int)] that returns a list of such triples  whose components
--   are at most the given limit

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6 A positive integer is perfect if it equals the sum of all its factors.
--   excluding the number itself. Using a list comprehension and the function factors
--   define a function perfects :: Int -> [Int] that returns a list of all perfect numbers
--   up to a given limit.

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x ]

-- 7 Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with 
--   two generators can be reexpressed using two comprehensions with single 
--   generators. Hint: nest one of the comprehensions within the other and use 
--   the library function concat :: [[a]] -> [a]

nested :: [(Int, Int)]
nested = [x | x <- concat [[(1,3), (1,4)], [(2,3),(2,4)]]] -- hack?

-- 8 Redefine the function positions using find


-- 9 The scalar product of two lists of ntegers xs and ys is given by 
--   the sum of the products of the corresponding integers (x1*y1) 

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

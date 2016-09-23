
-- 1. Using library functions, define a function halve :: [a] -> ([a],[a])
--    that splits an even lengthed list into two halves.

skip :: Int -> [a] -> [a]
skip n xs = reverse (take n (reverse xs)) 

halve :: [a] -> ([a],[a])
halve xs = ((take half xs), (skip half xs))
    where half = length xs `div` 2

-- 2 Define a function third :: [a] -> a
--   that returns the third element from a list Using
--   - head, tail
--   - !!
--   - pattern matching

third :: [a] -> a
--third xs = head (tail (tail xs))
--third xs = xs !! 2
third (_:_:x:xs) = x

-- 3 Consider a function safetail :: [a] -> [a] that behaves the same way as tail, 
--   but maps the empty list to itself instead of throwing an error. Define safe tail using:
--   - a conditional expression
--   - guarded equations
--   - pattern matching

safetail :: Eq a => [a] -> [a]
-- conditional
--safetail xs = if null xs then [] else tail xs
-- guard equations
--safetail xs | xs == []  = []
--            | otherwise = tail xs
-- pattern matching
safetail [] = []
safetail (x:xs) = xs

-- 4 In a siilar way to && in section 4.4, show how the disjunction operator ||
--   can be defined four different ways using pattern matching

(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

-- 5 Without using any other library functions or operators, 
--   show how the meaning of th following pattern matching definition
--   for the logical conjunction && can be formalized using conditional expressions

-- True && True = True
-- _    && _    = False

(&&) :: Bool -> Bool -> Bool
(&&) x y =
    if x == True then
        if y == True then True
        else False
    else False

-- 6 Do the same for te following alternative definition, and note the difference
--   in the number of conditional expressions that are required
-- True  && b = b
-- False && _ = False

(<&&>) :: Bool -> Bool -> Bool
(<&&>) x y =
    if x == True then y
    else False

-- 7 Show how the following curried function definition can be formalized
--   in terms of lanbda expressions
-- mult :: Int -> Int -> Int -> Int
-- mult z y z = x * x * z
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

-- 8 
luhnDouble :: Int -> Int
luhnDouble x = if dbl <= 9 then dbl else dbl - 9
    where dbl = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = total `mod` 10 == 0
    where 
        total = sum [(luhnDouble a), b,(luhnDouble c), d]

 
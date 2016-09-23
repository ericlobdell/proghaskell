
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



-- 2. Write down definition to th following types, it doesn't matter what they do, as long as they are type corect.

--bools :: [Bool]
bools = [True, False]

--nums :: [[Int]]
nums = [[1,2], [2,3]]

--add :: Int -> Int -> Int -> Int
add x y z = x + y + z

--copy :: a -> (a,a)
copy x = (x,x)

--apply :: (a->b) -> a -> b
apply f a = f a

-- 3. What are the types of the following functions

-- second xs = head (tail xs)
second :: [a] -> a

-- swap (x, y) = (y, x)
swap :: (a,b) -> (b,a)

-- pair x y = (x,y)
pair :: a -> b -> (a,b)

-- double x = x * 2
double :: Num a => a -> a

-- palindrome xs = reverse xs == xs
palindrome :: Eq a => [a] -> Bool

-- twice f x = f (f x)
twice :: (a -> a) -> a -> a



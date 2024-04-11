-- Chapter 1 Solutions
-- Q1
-- Solved on paper...

-- Q2
-- To prove, sum [x] = x
-- sum [] = 0
-- sum (x : xs) = x + sum xs
--
-- sum [x] = sum (x : []) = x + sum [] = x + 0 = x

-- Q3
-- Solved in paper...

-- Q4 
-- Comparison signs are reversed

-- Q5
-- It would remove the duplicates in the sorted list

double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]

-- Chapter 2 Solutions
-- Q1 and Q2 are tirivial
-- average ns = sum ns `div` length ns

-- Q3
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]


-- Q4
-- (take 1) = head --
lastFromList = head . reverse

-- Q5
removeLastFromList = reverse . tail . reverse
reverseLastFromListAlt [] = []
removeLastFromListAlt xs = take ((length xs) - 1) xs


-- randomCompute x y = x / y

-- Implementing zip using pattern matching
--zipp :: [a] -> [b] -> [(a, b)]
--zipp _ [] = []
--zipp [] _ = []
--zipp (x: xs) (y: ys) = [(x,y)] ++ zipp xs ys

-- Chapter 3 Solutions
-- Q1
a = ['a', 'b', 'c']
b = ('a', 'b', 'c')
c = [(False, '0'), (True, '1')]
d = ([False, True], ['0', '1'])
e = [tail, init, reverse]

-- Q2
bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1,2,3],[1]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy a = (a, a)

apply :: (a->b) -> a -> b
apply f a = f a

-- Q3
-- [x] -> x
-- (x,y) -> (y,x)
-- x -> y -> (x, y)
-- Num x => x -> x
-- Eq x => [x] -> Bool
-- (a -> a) -> a -> a

-- Q4 -- check the previous solns

-- Q5
-- Let us consider that function equality check is feasible
--
-- Base case:
-- adda :: Num -> Num
-- adda x = x + 1
-- adda = \x -> x + 1 
--
-- addb :: Num -> Num
-- addb m = m + 1
-- addb = \m -> m + 1
--
-- adda and addb point to two different lambda expressions, hence to determine equality we must compare lambda expressions.
--
-- Case1: \x -> x + 1
-- 
-- Case2: \m -> m + 1
-- 
-- Let us consider a naive algorithm that performs, formal renaming left to right.
--
-- Case1: \t -> t + 1
--
-- Case2: \t -> t + 1
--
-- Hence we were able to compare the two functions for equality, but does this hold true under the following?
--
-- 1. Can two equal functions always be compared trivially?
--
-- Case1: Structural difference
--
-- adda x = 1 + x
-- addb x = x + 1
--
-- Case1: \t -> 1 + t
-- Case2: \t -> t + 1
--
-- Equality determination now depends on reassociation of expressions. A function, in terms of input -> output relation is non trivial to detmine.
-- Properties such as input and output types may be compared, but this is not sufficient as different functions may share the same domain and range space.
--

-- Chapter 4 solutions
-- Q1
halve :: [a] -> ([a], [a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

-- Q2
third :: [a] -> a

--third = head.tail.tail

--third xs = xs !! 2 

third (x : y: z: xs) = z

-- Q3
safetail :: [a] -> [a]

--safetail xs = if null xs then [] else tail xs

--safetail xs | null xs   = []
--            | otherwise = tail xs

safetail [] = []
safetail (x:xs) = xs

-- Q4
(|||) :: Bool -> Bool -> Bool

--True ||| True = True
--True ||| False = True
--False ||| True = True
--False ||| False = False

--False ||| False = False
--_     ||| _      = True

False ||| b = b
True  ||| _ = True

-- Q5
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a == True then if b == True then True else False else False

-- Q6
(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a == True then b else False
-- Only one conditional is required

-- Q7
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-- Q8 (Luhn Algorithm)
luhnDouble :: Int -> Int
luhnDouble x | x + x > 9 = (x + x) - 9
             | otherwise = x + x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (sum [luhnDouble a, b, luhnDouble c, d]) 10 == 0


-- Chapter 5 Solutions

-- Concat implementation in haskell is neat
concatt :: [[a]] -> [a]
concatt xss = [x | xs <- xss, x <- xs]


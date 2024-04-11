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

-- Q1
c5q1 = sum [x^2 | x <- [1..100]]

-- Q2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Q3
square :: Int -> [(Int,Int)]
square n = [(x,y) | x <- [0..n], y <- [0..n], x /= y]

-- Q4
-- length :: [a] -> a
-- length [] = 0
-- length (x:xs) = 1 + length xs
replicateCPY :: Int -> a -> [a]
replicateCPY n a = [a | x <- [1..n]]

-- Q5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Q6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (excludedFactors x) == x] where
             excludedFactors m = [f | f <- factors m, f /= m]

-- Q7
-- This comprehension is expressed using two generators [(x,y) | x <- [1,2], y <- [3,4]]
-- 
-- Re-express it using two comprehensions with a single generator
someComprehension = concat [ [(x, y) | x <- [1,2]] | y <- [3,4]]

-- Q8
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v' | (k', v') <- t, k' == k]

positions :: Eq a => a -> [a] -> [Int]
positions v vs = find v (zip vs [0..])

-- Q9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ xi * yi | (xi, yi) <- zip xs ys ]

-- Q10
-- 
-- See caesarUpper.hs, a three line change is needed to support upper case letters
--
-- $ diff caesar.hs caesarUpper.hs 
-- 5a6,7
-- > let2intUpper c = ord c - ord 'A'
-- > 
-- 8a11
-- > int2letUpper n = chr (ord 'A' + n)
-- 11a15
-- >           | isUpper c = int2letUpper (((let2intUpper c) + n) `mod` 26)
-- 49,50c53
-- <            table' = freqs xs
-- < 
-- ---
-- >            table' = freqs [ toLower x | x <- xs ]


-- Chapter 6 Solutions

-- Q1
factorialHandleRec :: Int -> Int
factorialHandleRec n | n > 0 = n * factorialHandleRec (n-1)
                     | n == 0 = 1

-- Q2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- Q3
(^^^) :: Int -> Int -> Int
0 ^^^ _ = 0
1 ^^^ _ = 1
m ^^^ 1 = m
m ^^^ n = m * (m ^^^ (n - 1))

-- Q4
euclid :: Int -> Int -> Int

euclid m n | m == n = m
           | m > n  = euclid (m - n) n
           | otherwise = euclid n m

-- Q5
-- length [1,2,3] = length (1: (2: (3: [])))
--                = 1 + length (2 : (3: []))
--                = 1 + 1 + length (3 : [])
--                = 1 + 1 + 1 + length []
--                = 1 + 1 + 1 + 0

-- drop 3 [1,2,3,4,5] = drop 3 [1,2,3,4,5]
--                    = drop 2 [2,3,4,5]
--                    = drop 1 [3,4,5]
--                    = drop 0 [4,5]
--                    = [4,5]

-- init [1,2,3] = init (1: (2: (3: [])))
--              = 1 : init (2: (3: []))
--              = 1 : 2 : init [3] = 1 : 2 : init[_]
--              = 1 : 2 : []
--

-- Q6 
q6and :: [Bool] -> Bool
q6and [True] = True
q6and [False] = False
q6and (x:xs) | x == False = False
             | otherwise  = q6and xs

q6concat :: [[a]] -> [a]
q6concat [] = []
q6concat ([]:xss) = q6concat xss
q6concat ((x:xs):xss) = x : q6concat (xs : xss)
--q6concat xss = [x | xs <- xss, x <- xs ]


q6replicate :: Int -> a -> [a]
q6replicate 0 _ = []
q6replicate n a = a : q6replicate (n-1) a

q6SelectNth :: [a] -> Int -> a
q6SelectNth (x:xs) 0 = x
q6SelectNth (x:xs) n = q6SelectNth xs (n - 1)

q6elem :: Eq a => a -> [a] -> Bool
q6elem _ [] = False
q6elem a (x:xs) | a == x = True
                | otherwise = q6elem a xs

-- Q7

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Q8

q8halve :: [a] -> ([a],[a])
q8halve xs = (take l xs, drop l xs)
             where 
               l = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort lHalf) (msort rHalf) where
               (lHalf, rHalf) = q8halve xs


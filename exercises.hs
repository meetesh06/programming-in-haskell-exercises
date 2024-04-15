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

-- Q9 (TODO)

-- Chapter 7
-- snoc
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

-- Q1
-- [ f x | x <- xs, p x]
c7q1 f p = map f . filter p

-- Q2
-- a. (There is a typo in the book for this question)
c7all :: (a -> Bool) -> [a] -> Bool
c7all p = and . map p

-- b. (There is a typo in the bokk for this question)
c7any :: (a->Bool) -> [a] -> Bool
c7any p = or . map p

-- c.
c7takeWhile :: (a -> Bool) -> [a] -> [a]
c7takeWhile p = filter p

-- d.
c7dropWhile :: (a -> Bool) -> [a] -> [a]
c7dropWhile p = filter (not . p)

-- Q3
c7map f = foldr (\a b -> f a : b ) []
c7filter f = foldr (\a b -> if f a then a : b else b) []

-- Q4
c7dec2int :: [Int] -> Int
c7dec2int = foldl (\a b -> a * 10 + b) 0

-- Q5
c7curried :: ((a,b) -> c) -> (a -> b -> c)
c7curried f = \x -> \y -> f (x,y)

-- Q6
c7unfold p h t x | p x       = []
               | otherwise = h x : c7unfold p h t (t x)

c7int2bin = c7unfold (== 0) (`mod` 2) (`div` 2)
-- Chapter had type for Bit
c7chop8 :: [Int] -> [[Int]]
c7chop8 = c7unfold (== []) (take 8) (drop 8)

c7q6map :: (a -> b) -> [a] -> [b]
c7q6map funn = c7unfold null (funn . head) (tail)

c7q6iterate :: (a -> a) -> a -> [a]
c7q6iterate = c7unfold (const False) id 

-- Q7, Q8
-- See binaryStringTransmitterParity.hs

-- Q9 

altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap f g (x:y:xs) = f x : g y : altMap f g xs
altMap f g (x : []) = f x : []
altMap f g [] = []

-- Q10
--luhnDouble :: Int -> Int
--luhnDouble x | x + x > 9 = (x + x) - 9
--             | otherwise = x + x

luhn1 :: [Int] -> Bool
luhn1 xs = mod (sum (altMap id luhnDouble (reverse xs))) 10 == 0

-- Chapter 8 Solutions
--
-- 8.1 Type declaration
--
-- Declaring types using existing types
--
-- type String = [Char]
--
-- type Pos = (Int,Int)
-- type Trans = Pos -> Pos
--
-- type declarations cannot be rescursiv, the following is disallowed
-- type Tree = (Int,[Tree])
--   // If recursive types are needed, they can be added using the data mechanism'
--
-- Type declrations can also be parameterized by other types.
-- Ex: If we are defining many functions that manipulate pairs of the same type we can define 
-- type Pair a = (a,a)
--
-- Type declarations with more than one parameter are also allowed
-- Ex: A type of lookup tables that associate keys of one type to values of another type:
--
-- type Assoc k v = [(k,v)]
--
-- find :: Eq k => k -> Assoc k v -> v
-- find k t = head [v | (k',v) <- t, k == k']
--
-- 8.2 Data declarations
--
-- A completely new type.
--
-- The following declares that the type Bool comprises two new values, True and False.
--
-- data Bool = True | Flase
--   // here '|' is or and the values are called 'constructors' (they must be capitalized),
--      also more than one construtor name cannot be used in more than one type.
--  
-- data Move = North | South | East | West
--
-- move :: Move -> Pos -> Pos
-- move North (x,y) = (x,y+1)
-- move South (x,y) = (x,y-1)
-- move East (x,y)  = (x+1,y)
-- move West (x,y)  = (x-1,y)
--
-- moves :: [Move] -> Pos -> Pos
-- moves [] p = p
-- moves (m:ms) p = moves ms (move m p)
--
-- rev :: Move -> Move
-- rev North = South
-- rev South = North
-- rev East = West
-- rev West = East
--
-- The constructors of data declaration can have arguments
-- data Shape = Circle Float | Rect Float Float
--
-- square :: Float -> Shape
-- square n = Rect n n
--
-- area :: Shape -> Float
-- area (Circle r) = pi * r^2
-- area (Rect x y) = x * y
--
-- The difference between normal functions and constructors in Haskell is that the latter have no defining equations.
-- For instance the equation `negate 1.0` can be evaluated to `-1.0`, the expression `Rect 2 3` is already fully evaluated.
--
-- data constructors themselves can have arguments as follows.
-- data Maybe a = Nothing | Just a
--
--   // The above type states that the value of type `Maybe a` is either `Nothing` or `Maybe a`
--
-- safediv :: Int -> Int -> Maybe Int
-- safediv _ 0 = Nothing
-- safediv a b = Just (a `div` b)
--
-- safehead :: [a] -> Maybe a
-- safehead [] = Nothing
-- safehead (x:xs) = Just x
--
-- 8.3 Newtype Declarations
-- If a new type has a single construstor with a single argument, then it can be declared using the newtype mechanism.
--
-- newtype Nat = N Int
--   // The constructor N takes a single argument of type Int (the task of ensuring its non-negative is upto the programmer)
--
-- How is this any different from the following?
--
-- type Nat = Int
-- data Nat = N Int
--
-- First, `newtype` rather than type means that `Nat` and `Int` are different (useful property to catch more bugs statically)
-- Second, using `newtype` rather than `data` brings efficiency benefit, because `newtype` constructors such as `N` do not incur
--   any cost when the programs are evaluated, as they are automatically removed by the compiler once the type checking is completed.
--   In short, `newtype` helps improve type safety without affecting performance.
--
-- 8.4 Recursive Types
--
-- New types declared using `data` and `newtype` can also be recursive.
--
-- data Nat = Zero | Succ Nat
--
-- nat2int :: Nat -> Int
-- nat2int Zero = 0
-- nat2int (Succ x) = 1 + nat2int x
--
-- int2nat :: Int -> Nat
-- int2nat 0 = Zero
-- int2nat n = Succ (int2nat (n - 1))
--
-- add :: Nat -> Nat -> Nat
-- add x y = int2nat (nat2int x + nat2int y)
--
-- // or
--
-- addNat :: Nat -> Nat -> Nat
-- addNat Zero y = y
-- addNat (Succ x) y = Succ (addNat x y)
--
-- Declaring our own lists
-- data List a = Null | Cons a (List a)

-- testObj :: List Int
-- testObj = Cons 10 (Cons 11 Null)

-- len :: List Int -> Int
-- len Null = 0
-- len (Cons _ b) = 1 + len b
--
-- data Tree a = Leaf a | Node (Tree a) a (Tree a)
--               deriving (Eq, Ord)
-- t :: Tree Int
-- t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- occurs :: Eq a => a -> Tree a -> Bool
-- occurs a (Leaf x) = a == x
-- occurs a (Node lt x rt) = a == x || occurs a lt || occurs a rt 

-- flatten :: Tree a -> [a]
-- flatten (Leaf x) = [x]
-- flatten (Node lt x rt) = flatten lt ++ [x] ++ flatten rt 

-- occursBS :: Ord a => Eq a => a -> Tree a -> Bool
-- occursBS a (Leaf x) = a == x
-- occursBS a (Node lt x rt) | a == x    = True
--                          | a > x     = occursBS a rt
--                          | otherwise = occursBS a lt
--
-- 8.5 Class and instance declarations
--
-- The following code shows how the class `Eq` is declared in Haskell.
--
-- class Eq a where
--   (==), (/=) :: a -> a -> Bool
--
--   x /= y = not (x == y)
--
-- In the eq class, we dont need to define `/=` because it is already defined.
-- 
-- Instance Eq Bool where
--   False == False = True
--   True  == True  = True
--   _     == _     = False
--
-- Only types that are declared using `data` and `datatype` mechanisms can be made into instances
-- of classes (default definitions can be overridden by instance declarations if desired.
--
--
-- The following shows how the class `Ord` extends `Eq`.
--
--class Eq a => Ord a where
--  (<), (<=), (>), (>=) :: a -> a -> Bool
--  min, max             :: a -> a -> a
--  min x y | x <= y = x
--          | otherwise = y
--  max x y | x <= y = y
--          | otherwise = x

 
-- Defining `Bool` as a `Ord` type requires the following instantiation.
--

-- data MBool = MTrue | MFalse 

-- instance Eq MBool where
--   MTrue == MTrue  = True
--   MTrue == MFalse = True
--   _     == _      = False

-- instance Ord MBool where
--   MFalse < MTrue = True
--   _      < _    = False
  
--   a <= b = a == b || a < b
--   a > b  = b < a
--   a >= b = a == b || a > b


-- Subsection: Derived Instances
--
-- In the standard prelude the type for Bool is declared as
-- data Bool = False | true
--             deriving (Eq, Ord, Show, Read)
--
-- data Shape = Circle Float | Rect Float Float
--              deriving (Eq, Ord)



-- Solutions begin here
-- Q1
data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int x
addNat :: Nat -> Nat -> Nat
addNat Zero x = x
addNat (Succ x) y = addNat x (Succ y)
multNat :: Nat -> Nat -> Nat
multNat Zero _ = Zero
multNat _ Zero = Zero
multNat (Succ Zero) x = x
multNat (Succ x)    y = addNat y (multNat x y)

-- Q2
-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a-> Ordering

-- Default implementation using guards
-- occurs x (Node l y r) | x == y = True
--                       | x < y  = occurs x l
--                       | otherwise = occurs x r

-- Version using pattern matching
-- This versions is better because it always only requires one comparison
-- while the previous version might have required two comparisons
occurs :: Ord a => a -> Tree a -> Bool
data Tree a = Leaf a | Node (Tree a) a (Tree a)
occurs x (Leaf y)   = x == y
occurs x (Node l y r) = case compare x y of
                        EQ -> True
                        LT -> occurs x l
                        GT -> occurs x r  
-- Q3
data Q3Tree a = Q3Leaf a | Q3Node (Q3Tree a) (Q3Tree a)
                deriving Show
numLeaves :: Q3Tree a -> Int
numLeaves (Q3Leaf _) = 1
numLeaves (Q3Node x y) = numLeaves x + numLeaves y

balanced :: Q3Tree a -> Bool

balanced (Q3Leaf _) = True
balanced (Q3Node lt rt) = abs (numLeaves lt - numLeaves rt) <= 1 && balanced lt && balanced rt

-- Q4
split :: [a] -> ([a],[a])
split xs = (take half xs, drop half xs) where half = length xs `div` 2

balance :: [a] -> Q3Tree a
balance [x] = (Q3Leaf x)
balance xs = Q3Node (balance leftHalf) (balance rightHalf) where (leftHalf,rightHalf) = split xs

-- Q5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a->a->a) -> Expr -> a
folde f g (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Q6
eval :: Expr -> Int 
eval = folde (\x -> x) (\x y -> x + y)

size :: Expr -> Int
size = folde (\_ -> 1) (\x y -> x + y)

-- Q7
data MMaybe a = MNothing | MJust a

instance Eq a => Eq (MMaybe a) where
  MNothing == MNothing   = True
  (MJust x) == (MJust y) = x == y
  _        == _        = False

data MList a = Null | Cons a (MList a)                                                                                                                                  
-- Using a different list class, as Eq a => Eq [a] is already defined in GHC.Classes
instance Eq a => Eq (MList a) where
  Null == Null = True
  Null == _    = False
  _  == Null   = False
  (Cons x xs) == (Cons y ys) = (x == y) && (xs == ys)

-- l = Cons 10 (Cons 11 Null)
-- r = Cons 10 (Cons 11 Null)
-- l == r

-- Q8, Q9


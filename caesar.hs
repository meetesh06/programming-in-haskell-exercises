import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)


shift :: Int -> Char -> Char
shift n c | isLower c = int2let (((let2int c) + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count c cs = length [x | x <- cs, x == c]

freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- ['a'..'z']]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((oi - ei)^2/ei) | (oi, ei) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [n | (m, n) <- zip xs [0..], m == x]

crack xs = encode (-factor) xs
         where
           factor = head (positions (minimum chitab) chitab)
           chitab = [chisqr (rotate n table') table | n <- [0..25]]
           table' = freqs xs


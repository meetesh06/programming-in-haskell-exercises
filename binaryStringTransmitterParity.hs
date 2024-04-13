import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
--
--My implementation
--
--bin2int bs = sum [ b * p | (b, p) <- zip bs powers ] where
--             powers = [2^p | p <- [0..]]

-- The integrate higher order function produces an infinite list by applying
-- a function an increasing number of times to a value:
--
-- integrate f x = [x, f x, f (f x), f (f (f x)), ...]

integrate :: (a -> a) -> a -> [a]
integrate f x = [ (foldr (\a b -> f.b) id [1..t]) x | t <- [0..]]

--bin2int bits = sum [ w * b | (w, b) <- zip weights bits ] where
--               weights = integrate (*2) 1


bin2int = foldr (\l r -> l + 2 * r) 0 

int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bs = take 8 (bs ++ repeat 0)

count :: Eq a => a -> [a] -> Int
count a = foldr (\x y -> if x == a then 1 + y else y) 0 

addParityBit :: [Bit] -> [Bit]
addParityBit xs | odd (count 1 xs) = xs ++ [1]
                | otherwise        = xs ++ [0]

encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits) 

parityCheck :: [[Bit]] -> [[Bit]]
parityCheck [] = []
parityCheck (x:xs) | odd (count 1 x) = error "Parity Check Failed"
                   | otherwise        = (take 8 x) : parityCheck xs

decode :: [Bit] -> String
-- decode bits = [ chr (bin2int b)  | b <- chop8 bits ]
decode = (map (chr.bin2int)) . parityCheck . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
-- Ideal Channel
channel = id

-- Faulty Channel
-- channel = tail




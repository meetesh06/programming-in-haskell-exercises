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

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits) 

decode :: [Bit] -> String
-- decode bits = [ chr (bin2int b)  | b <- chop8 bits ]
decode = (map (chr.bin2int)). chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

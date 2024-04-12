import Data.List
--
-- Section 7.7
--

-- First Past the post algotihm

count :: Eq a => a -> [a] -> Int
count a [] = 0
count a (x:xs) | (x == a) = 1 + count a xs
               | otherwise = count a xs

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
--rmdups (x:xs) = x : rmdups (filter (\a -> a /= x) xs)
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [ (count c vs, c) | c <- rmdups vs ] 

winner :: Ord a => [a] -> a
winner =  snd . last . result


-- Alternative Vote

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green","Red","Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
             [x] -> x
             (c:cs) -> winner' (elim c bs) 

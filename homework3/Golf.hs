module Golf where

-- nasty and indexy, make better!
skips :: [a] -> [[a]]
skips xs = [[xs !! (i-1) | i <- [1..length xs], i `mod` n == 0] | n <- [1..length xs]]


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) = if x < y && z < y then y:(localMaxima $ z:zs) else localMaxima (y:z:zs)
localMaxima _ = []


-- histogram :: [Integer] -> String
-- histogram xs = map (flip replicate '*' . count xs) [0..9] 
--     where count [] _ = 0
--           count (x:xs) n = (if x == n then 1 else 0) + count xs n
-- 

-- need to left pad with spaces to make tranpose work, have numbers counting down from top
histogram :: [Integer] -> String
histogram xs =  unlines $ map (flip replicate '*' . count xs) [0..9] ++ [replicate 10 '=', ['0'..'9']]
    where count [] _ = 0
          count (x:xs) n = (if x == n then 1 else 0) + count xs n

count :: [Int] -> Int -> Int
count [] _ = 0
count (x:xs) n = (if x == n then 1 else 0) + count xs n

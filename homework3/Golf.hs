module Golf where

-- nasty and indexy, make better!
skips :: [a] -> [[a]]
skips xs = [[xs !! (i-1) | i <- [1..length xs], i `mod` n == 0] | n <- [1..length xs]]


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) = if x < y && z < y then y:(localMaxima $ z:zs) else localMaxima (y:z:zs)
localMaxima _ = []


histogram :: [Integer] -> String

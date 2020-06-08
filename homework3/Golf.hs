module Golf where

-- skips :: [a] -> [[a]]


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) = if x < y && z < y then y:(localMaxima $ z:zs) else localMaxima (y:z:zs)
localMaxima _ = []


-- histogram :: [Integer] -> String

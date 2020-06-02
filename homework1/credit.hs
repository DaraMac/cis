module Credit where

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = [read x :: Integer | x <- (map (:[]) (show n))]


toDigitsRev :: Integer -> [Integer]
toDigitsRev  = reverse . toDigits


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = skipDub (reverse ns)
    where skipDub (x:y:xs) = skipDub xs ++ [2*y, x]
          skipDub xs       = xs


sumDigits :: [Integer] -> Integer
sumDigits ns = sum [if n > 9 then n - 9 else n | n <- ns]


validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0

{-# OPTIONS_GHC -Wall #-}

import Data.List

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 []         = 1
fun1 (x:xs)
    | even x    = (x - 2)*fun1 xs
    | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' = product . map  ((-) 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n    = n + fun2 (n `div` 2)
        | otherwise = fun2 (3*n + 1)


fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3*n + 1)


-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree = foldr (balance . attach) Leaf
    where
        height Leaf             = -1
        height (Node h _ _ _)   = h
        attach x Leaf           = Node 0 Leaf x Leaf
        attach x (Node h l a r) = if height l < height r
                                  then Node h (attach x l) a r
                                  else Node h l a (attach x r)

        balance Leaf             = Leaf
        balance n@(Node h l a r) = Node (calc n) (balance l) a (balance r)
        calc Leaf               = -1
        calc (Node _ l _ r)     = 1 + maximum [calc l, calc r]

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (/=) False


map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- dont think this is right, find example that breaks
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base xs


-- Exercise 4

sieveSundram' :: Integer -> [Integer]
sieveSundram' n = map ((+1).(*2)) $ [1..n] \\ [i+j+2*i*j | i <- [1..n], j <- [1..n]]

--sieveSundram :: Integer -> [Integer]
--sieveSundram =

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

{-# OPTIONS_GHC -Wall -fno-warn-missing-methods -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)


-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList


-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)


-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs

ruler :: Stream Integer
ruler = foldr interleaveStreams undefined $ map streamRepeat [0..]


-- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 $ streamRepeat 0)

instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate = streamMap negate
    (+) (Cons a as) (Cons b bs) = Cons (a+b) (as+bs)
    (*) (Cons a as) bb@(Cons b bs) = Cons (a*b) $ (streamMap (*a) bs) + (as*bb)


instance Fractional (Stream Integer) where
    (/) aa@(Cons a as) bb@(Cons b bs) = Cons (div a b) $ streamMap (`div` b) $ (aa/bb)*(as - bs)

-- 0, 1, 1, 2, 3, 5, 8, 13
-- f x = x / (1 - (x + x^2))


-- Exercise 7

data Matrix = M {a::Integer, b::Integer, c::Integer, d::Integer}
    deriving Show

instance Num Matrix where
    (*) (M a b c d) (M e f g h) = M (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

f1 :: Matrix
f1 = M 1 1 1 0

fib4 :: Integer -> Integer
fib4 n = if n == 0
         then 0
         else b (f1^n)

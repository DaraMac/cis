{-# OPTIONS_GHC -Wall #-}
module Calc where
import ExprT
import Parser


-- Exercise 1

eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Add s t) = eval s + eval t
eval (Mul s t) = eval s * eval t


-- Exercise 2

evalString :: String -> Maybe Integer
evalString s = case parseExp Lit Add Mul s
               of Just a  -> Just (eval a)
                  Nothing -> Nothing


-- Exercise 3

class Expr a where
    lit :: Integer -> a
    add, mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

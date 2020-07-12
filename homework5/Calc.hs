{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Calc where
import ExprT
import Parser
import StackVM()
-- import qualified StackVM as S

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


reify :: ExprT -> ExprT
reify = id


-- Exercise 4

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)


newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax m) (MinMax n) = MinMax (max m n)
    mul (MinMax m) (MinMax n) = MinMax (min m n)


newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 m) (Mod7 n) = Mod7 $ (m+n) `mod` 7
    mul (Mod7 m) (Mod7 n) = Mod7 $ (m*n) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- commenting out so I dont have to see compiler warnings
-- testInteger     = testExp :: Maybe Integer
-- testBool        = testExp :: Maybe Bool
-- testMM          = testExp :: Maybe MinMax
-- testSat         = testExp :: Maybe Mod7


-- Exercise 5

-- instance Expr Program where
--    lit = (:[]) . PushI

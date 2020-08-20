{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)


-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r


-- Exercise 2

-- 1

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a

indexJ _ Empty          = Nothing
indexJ i _     | i < 0  = Nothing
indexJ 0 (Single _ a)   = Just a
indexJ _ (Single _ _)   = Nothing
indexJ i (Append s l r)
    | Size i >= size s  = Nothing 
    | i >= leftSize     = indexJ (i - leftSize) r
    | otherwise         = indexJ i l
    where leftSize = getSize $ size $ tag l


-- 2

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a

dropJ _ Empty          = Empty
dropJ i js | i <= 0    = js
dropJ _ (Single _ _)   = Empty
dropJ i (Append b l r)
    | Size i >= size b = Empty
    | i >= leftSize    = dropJ (i - leftSize) r
    | otherwise        = (dropJ i l) +++ r
    where leftSize = getSize $ size $ tag l


-- 3
takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a

-- TODO double check all cases make sense as I copied them from above
takeJ _ Empty           = Empty
takeJ i _ | i <= 0      = Empty
takeJ _ j@(Single _ _)  = j
takeJ i js@(Append b l r)
    | Size i >= size b  = js
    | i > leftSize      = l +++ takeJ (i - leftSize) r
    | otherwise         = takeJ i l
    where leftSize = getSize $ size $ tag l











-----------------------------------------------------
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

rightTest :: JoinList Size Char
rightTest = foldr (+++) Empty $ map (Single (Size 1)) "abcde"

leftTest :: JoinList Size Char
leftTest = foldl (+++) Empty $ map (Single (Size 1)) "abcde"

midTest :: JoinList Size Char
midTest = Append (Size 5) (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b'))
                          (Append (Size 3)
                              (Append (Size 2) (Single (Size 1) 'c') (Single (Size 1 ) 'd'))
                              (Single (Size 1) 'e'))
------------------------------------------------------

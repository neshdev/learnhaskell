module Recursion862 where

sums :: (Eq a, Num a) => a -> a
sums 0 = 0
sums a = a + sums(a-1)

mults :: (Show a, Integral a) => a -> a -> a
mults a b 
    | b == 1    = a
    | otherwise = a + mults a (b-1)
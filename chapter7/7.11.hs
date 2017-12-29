module Chapter7Excercises where

--1
tensDigit :: Integral a => a -> a
tensDigit x = d
   where xLast = x `div` 10
         d     = xLast `mod` 10
--dontknow how to combine divMod in one go

--2
foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True  = y

foldBool1 :: a -> a -> Bool -> a
foldBool1 a1 a2 cond = case cond of
    True -> a2
    False -> a1


foldBool2 :: a -> a -> Bool -> a
foldBool2 a a' cond
   | cond == True = a'
   | cond == False = a

--3
g :: (a->b) -> (a,c) -> (b,c)
g f (a,c) = (f a, c)


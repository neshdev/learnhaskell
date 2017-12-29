module LearnChapter7 where

mTh x y z = x * y * z
mTh1 x y = \z -> x * y * z
mTh2 x = \y -> \z -> x * y * z
mTh3 = \x -> \y -> \z -> x * y * z


addFive x y = (if  x > y then y else x) + 5
addFive1 = \x -> \y -> (if  x > y then y else x) + 5

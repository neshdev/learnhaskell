module Chapter9Exercises where


t1 = [True,True]
t2 = [True,False]
t3 = [False,False]

odds =  [ x |x <- [1..5] , odd x ]

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)
   | not x      = False
   | otherwise  = myAnd xs


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
   | x         = True
   | otherwise = myOr xs


myAny :: (a->Bool) -> [a] -> Bool
myAny f xs = myOr (map f xs)

_1to10 = [1..10]
_2to10 = [2..10]

myElem :: Eq a => a -> [a] -> Bool
myElem a []    = False
myElem a (x:xs)
   | a == x    = True
   | otherwise = myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any f where
            f x = a == x

rev' :: [a] -> [a] -> [a]
rev' []     a = a
rev' (x:xs) a = rev' xs (x:a)

myReverse :: [a] -> [a]
myReverse xs = rev' xs []

squish :: [[a]] -> [a]
squish = myReverse . squish' []

combine :: [a] -> [a] -> [a]
combine [] ys = ys  
combine xs [] = xs
combine (x:xs) ys = x : combine xs ys

squish' :: [a] -> [[a]] -> [a]
squish' acc []                 = acc
squish' acc ([]:ys)            = squish' acc  ys
squish' acc ((x:xs):ys)        = squish' (combine (x:xs) acc)  ys


squishMap :: (a->[b]) -> [a] -> [b]
squishMap f xs = squish listOfLists where
                 listOfLists = map f xs

myMaximumBy' :: (a->a->Ordering) -> a -> [a] -> a
myMaximumBy' _ y []   = y
myMaximumBy' f y (x:xs)   
    | f x y == GT     = myMaximumBy' f x xs
    | otherwise       = myMaximumBy' f y xs


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = myMaximumBy' f x xs


add' :: (Num a ) => a -> a -> a
add' x y = x + y

main :: IO ()
main = print $ myMaximumBy compare [1,2,3,2,1]
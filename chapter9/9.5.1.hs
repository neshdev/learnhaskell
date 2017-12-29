module EnumFromToExercise where

eftBool :: Bool -> Bool -> [Bool]
eftBool t t'
   | t   && t'       = [True]
   | not t && t'     = [False,True]
   | not t && not t' = [False]
   | otherwise       = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT,EQ]
eftOrd LT GT = [LT,EQ,GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ,GT]
eftOrd GT GT = [GT]
eftOrd _ _ = []

eftInt :: Int -> Int -> [Int]
eftInt a b
   | a > b = []
   | otherwise = a : eftInt (succ a) b

eftChar :: Char -> Char -> String
eftChar a b
    | a > b = []
    | otherwise = a : eftChar (succ a) b
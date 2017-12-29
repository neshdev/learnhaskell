module DetermineTheType where

example = 1

-- #1
a1 = (* 9) 6

b1 = head [(0,"doge"),(1, "kitteh")]

c1 = head [(0,"doge"),(1, "kitteh")]

d1 = if False then True else False

e1 = length [1,2,3,4,5]

f1 = length [1,2,3,4,5] > length "TACOCAT"

-- #2
-- x = 5
-- y = x + 5
-- w = y + 10
-- what is the type of w?
-- w :: Num a => a

-- #3
-- x = 5
-- y = x + 5
-- z y = y * 10
-- what is the type of w?
-- z :: Num a => a -> a

-- #4
-- x = 5
-- y = x + 5
-- f = 4 / y
-- what is the type of f?
-- f :: Fractional a => a

-- #5
-- x = "Julie"
-- y = " <3 "
-- z = "Haskell"
-- f = x ++ y ++ z
-- what is the type of f?
-- f :: [Char]

{-| 
    Type variable of specific type constructor:
    fully polymorphic type variable, constrained polymorphic type variable, concrete type constructor
    1. f :: Num a => a -> b -> Int -> Int
                    [0]  [1]  [2]    [3]
    [0] :: constrained
    [1] :: polymorphic
    [2] :: concrete
    [3] :: concrete

    2. f :: zed -> Zed -> Blah
            [0]    [1]    [2]
    [0] :: polymorphic
    [1] :: concrete
    [2] :: concrete

    3. f :: Enum b => a -> b -> C
                      [0]  [2]  [3]
    [0] :: polymorphic
    [1] :: constrained
    [2] :: concrete

    4. f: f -> g -> C
          [0]  [1]  [2]
    [0] :: polymorphic
    [1] :: polymorphic
    [2] :: concrete
-}

-- Write the type signature
functionH :: [x] -> x
functionH (x:_) = x

functionC :: (Ord x, Ord y) => x -> y -> Bool
functionC x y = if x > y then True else False

functionS :: (a,b) -> b
functionS (x,y) = y
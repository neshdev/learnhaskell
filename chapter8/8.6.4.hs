module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = show n

digits :: Int -> [Int]
digits n = go n 10 where 
    go n div
        | n == 0 = []
        | otherwise = go q div ++ [r]
        where 
            (q,r) = divMod n div
    

wordNumber :: Int -> String
wordNumber n = concat dashes  where
    numbers = digits n
    numberToWords = map digitToWord numbers
    dashes = intersperse "-" numberToWords


module List1 where

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs


x = [1..10]
x' = enumFromTo 1 10

y = [1,2..10]
y' = enumFromThenTo 1 2 10

z = [1,3..10]
z' = enumFromThenTo 1 3 10

a = ['t'..'z']
a' = enumFromTo 't' 'z'
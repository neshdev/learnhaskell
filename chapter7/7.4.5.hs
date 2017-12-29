module TupleFunctions where

addEmUp2 :: Num a => (a,a) -> a
addEmUp2 (x,y) = x + y

addEmUp2' :: Num a => (a,a) -> a
addEmUp2' tup = (+) (fst tup) (snd tup)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

thrid3 :: (a,b,c) -> c
thrid3 (_,_,x) = x


--exercises

k :: (a,b) -> a
k (x,y) = x

k1 = k ((4-1),10)
k2 = k ("three", (1+2))
k3 = k (3,True)


f' :: (a,b,c) -> (d,e,f) -> ((a,d),(c,f))
f' (a,_,c) (d,_,f) = ((a,d),(c,f))
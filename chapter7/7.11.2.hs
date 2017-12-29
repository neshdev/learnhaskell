module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip1 :: (Show a, Read a) => a -> a
roundTrip1 a = read . show $ a

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 a = read . show $ a


main = do
    print (roundTrip 4)
    print (id 4)

    print (roundTrip1 4)
    print (id 4)

    print (roundTrip2 4 :: Int)
    print (id 4)
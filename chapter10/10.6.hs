module Chapter10 where
    
    import Data.Time
    import Data.List
    
    -- will throw an error
    --foldr (\a b -> undefined) "" ["Pizza", "Apple", "Banana"]
    
    -- pab = ["Pizza", "Apple", "Banana"]
    -- foldr (\a b -> take 3 a) "" pab == "Piz"
    
    --foldl (\b a -> take 3 a) "" pab == "Ban"
    
    
    data DatabaseItem = DbString String 
                      | DbNumber Integer 
                      | DbDate   UTCTime 
                      deriving (Eq, Ord, Show)
                      
    theDatabase :: [DatabaseItem]
    
    theDatabase =
     [
         DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
        ,DbNumber 1
        ,DbNumber 2
        ,DbNumber 3
        ,DbString "Hello, world!"
        ,DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
     ]
    
    
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f (DbDate t) b = t : b
    f _ b = b 
            
    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate xs = foldr f [] xs 
    
    f' :: DatabaseItem -> [Integer] -> [Integer]
    f' (DbNumber t) b = t : b
    f' _ b = b 
    
    filterDbNumber :: [DatabaseItem] -> [Integer]
    filterDbNumber xs = foldr f' [] xs 
    
    f'' :: UTCTime -> UTCTime -> UTCTime
    f'' t t' = min t t'
    
    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent xs = foldr f'' y ys where
                    (y:ys) = filterDbDate xs
    --mostRecent = undefined
    
    sum' :: Integer -> Integer -> Integer
    sum' x x' = x + x'
    
    sumDb :: [DatabaseItem] -> Integer
    sumDb xs = foldr sum' 0 numbers  where
               numbers = filterDbNumber xs
    
    temporalAvg :: (Fractional a, Num a)=> (Integer,Integer) -> a -> a;
    temporalAvg (x_t, t') prevAvg = (prevAvg * (t-1) + x't)/t where
                                   x't = fromIntegral x_t
                                   t = fromIntegral t'
                                   
    
    avgDb :: [DatabaseItem] -> Double
    avgDb xs = foldr temporalAvg 0.0 pairs where
               numbers = filterDbNumber xs
               counts = enumFromTo 1 (genericLength numbers)
               pairs = reverse (zip numbers counts)
    
                      
    main :: IO ()
    main = do
           print "init"
           let x = filterDbDate theDatabase
           let y = filterDbNumber theDatabase
           
           print "done"
    
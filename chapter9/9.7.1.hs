module Chapter97.Chipher where
    
    import Data.Char
    
    offset :: Int
    offset = ord 'a'
    
    numOfChars = length ['a'.. 'z']
    
    caesarNumber :: Int -> Char -> Int
    caesarNumber shiftLength letter = (((ord letter) - offset + shiftLength) `mod` (numOfChars)) + offset
    
    caesar :: Int -> [Char] -> [Char]
    caesar shiftLength message = [chr (caesarNumber shiftLength letter) | letter <- message]
    
    uncaesarNumber :: Int -> Char -> Int
    uncaesarNumber shiftLength letter = (((ord letter) - offset - shiftLength) `mod` (numOfChars)) + offset
    
    unCaesar :: Int -> String -> String
    unCaesar shiftLength message = [chr (uncaesarNumber shiftLength letter)  | letter <- message]
    
    main :: IO ()
    main = do
           print "init"
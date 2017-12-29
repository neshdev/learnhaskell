module Chapter where

triple :: (Num x) => x -> x
triple x = x * 3


foo x = 
    let y = x * 2
        z = x ^ 2
    in 2 * y * z

main :: IO ()
main = do
    putStrLn("test" ++ "hello")
    putStrLn("test" ++ "world")
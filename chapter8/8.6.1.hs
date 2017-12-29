module Recursion86 where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appendCatty :: String -> String
appendCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

main :: IO()
main = do
    putStrLn(appendCatty "woohoo!")
    putStrLn(frappe "1")
    putStrLn(frappe (appendCatty "2"))
    putStrLn(appendCatty (frappe "blue"))
    putStrLn(cattyConny (frappe "pink")
                        (cattyConny "green" (appendCatty "blue")))
    putStrLn(cattyConny (flippy "Pugs" "are") "awesome")
    
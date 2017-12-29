module HigherOrderFunctions where

data Employee =
      Coder
    | Manager
    | Veep
    | CEO
    deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
    putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank' :: Employee -> Employee -> IO ()
employeeRank' e e' =
    case compare e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither Employee is the boss"
        LT -> (flip reportBoss) e e'


employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither Employee is the boss"
        LT -> (flip reportBoss) e e'

codersRuleCeoDroops :: (Employee -> Employee -> Ordering)
codersRuleCeoDroops Coder Coder = EQ
codersRuleCeoDroops Coder _ = GT
codersRuleCeoDroops _ Coder = LT
codersRuleCeoDroops e e' = compare e e'
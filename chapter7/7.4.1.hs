module RegisteredUser where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO()
printUser UnregisteredUser = putStrLn "Unregistered"
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) = putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive = 
      Galapagos
    | Antartica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

newtype Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAmerica  = True
_ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) = ((b,d),(a,c))
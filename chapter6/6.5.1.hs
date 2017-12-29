module Learn where

data DayOfWeek =
    Mon | Tue | Wed | Thur | Fri | Sat | Sun deriving (Show, Ord)

data Date = 
    Date DayOfWeek  Int


instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thur Thur = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

instance Eq Date where
    (==) (Date weekDay dayOfMonth) (Date weekDay' dayOfMonth') = weekDay == weekDay' && dayOfMonth == dayOfMonth'


data TisAnInteger = TisAn Integer

--Eq Instance's Exercises
--1
instance Eq TisAnInteger where
    (==) (TisAn integer) (TisAn integer') = integer == integer'

--2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two int1 int2) (Two int1' int2' ) = int1 == int1' && int2 == int2'

--3 
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt int) (TisAnInt int') = int == int'
    (==) (TisAString string) (TisAString string') = string == string'
    (==) _ _ = False

--4
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2) (Pair a1' a2') = a1 == a1' && a2 == a2'

--5
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a1 a2) (Tuple a1' a2') = a1 == a1' && a2 == a2'

--6
data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

--7
data EitherOr a b =
    Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a1) (Hello a1') = a1 == a1'
    (==) (Goodbye b1) (Goodbye b1') = b1 == b1'
    (==) _ _ = False

data Mood = Woot | Blah deriving (Show,Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aTob a b = aTob a == b

--todo arith :: Num b => (a -> b) -> Integer -> a -> b
--todo arith aTob int a b = 
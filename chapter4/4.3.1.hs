module Chapter4_3 where

data Mood = Blah | Woot deriving Show
-- [1]      [2] [3] [4]
-- [1] - type constructor
-- [2] - data constructor for value Blah
-- [3] - logical disjunction OR
-- [4] - data constructor for value Woot
-- pattern matching
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah
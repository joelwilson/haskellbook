module ChangeMood where

data Mood = Blah | Woot deriving Show

changeMood Blah = Woot
changeMood    _ = Blah

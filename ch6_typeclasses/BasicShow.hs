module BasicShow where

data Mood = Blah

instance Show Mood where
  show _ = "Blah"



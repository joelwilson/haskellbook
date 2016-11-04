-- an exercise

-- setup
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 90020
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1 filter for DbDate vals. Return a list of their UTCTimes.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate d) xs = d : xs
        f _          xs = xs

-- 2 filter for DbNumber vals. Return list of integers inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber n) xs = n : xs
        f _            xs = xs

-- 3 get the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4 sum all DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5 avg of DbNumber values
avgDb :: [DatabaseItem] -> Double
avgDb xs = avg $ map fromIntegral $ filterDbNumber xs
  where avg xs = sum xs / (fromIntegral . length) xs


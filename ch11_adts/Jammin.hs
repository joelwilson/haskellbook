module Jammin where -- 1.

import Data.List

-- Exercises: Jammin
data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

-- 2.rewrite with record syntax
data JamJars =
  Jam { fruit :: Fruit
      , count :: Int }
      deriving (Eq, Show, Ord)

-- 3. what is the cardinality of JamJars?
-- Fruit = 4 (one for each fruit type)
-- Int   = 9223372036854775807 + 9223372036854775808 + 1
--         maxBound :: Int     + (negate (minBound :: Int)) + (the spot for 0)
-- Total = Int + 4

-- 4. Add Ord instances to deriving clauses
-- check.

-- 5. create test data representing the rows in a pantry.
--    map one or both field accessors over the list.
row1 = Jam Peach 12
row2 = Jam Plum  15
row3 = Jam Apple 20
row4 = Jam Blackberry 5
row5 = Jam Apple 6
row6 = Jam Peach 21
allJam = [row1, row2, row3, row4, row5, row6]

counts = map count allJam
names  = map fruit allJam

-- 6. Write a function to get the total number of jam jars
total :: [JamJars] -> Int
total = sum . map count

-- 7. function to return the max row
maxRow :: [JamJars] -> JamJars
maxRow (x:xs) = foldr (\x y -> if count x > count y then x else y) x xs

-- 8. import Data.List (has sortBy and groupBy) and look up their types.
-- Check.

-- 9. sort the jams by names. Use this helper function if you want.
compareKind (Jam k _) (Jam k' _) = compare k k'

sortedJams = sortBy compareKind allJam

-- 10. group the jams by the type of fruit their are made from.
groupedJams = groupBy sameKinds sortedJams
  where sameKinds (Jam k _) (Jam k' _) = k == k'

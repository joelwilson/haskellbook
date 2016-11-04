module GenExercises where
-- Make a Gen random generator for the datatype

import Test.QuickCheck

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

-- 1. Equal probabilities for each.
foolGenEqual :: Gen Fool
foolGenEqual = oneof [return Fulse,
                      return Frue]

--instance Arbitrary Fool where
--  arbitrary = foolGenEqual

-- 2. 2/3 chance of Fulse, 1/3 chance of Frue.
foolGenFavorFulse :: Gen Fool
foolGenFavorFulse =
  frequency [(1, return Frue),
             (2, return Fulse)]

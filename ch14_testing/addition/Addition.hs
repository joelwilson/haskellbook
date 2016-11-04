module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = go (n -d) d (count + 1)

-- for the exercise at the end of 14.3 (and associated tests)
multiply :: (Eq a, Num a) => a -> a -> a
multiply x y = go x y 0
  where go x' y' total
         | x' == 0 = total
         | otherwise = go (x' - 1) y' (total + y')

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > (1 :: Integer) `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` (4 :: Integer)
    it "15 divided by 3 is 5" $ do
      dividedBy (15 :: Integer) 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy (22 :: Int) 5 `shouldBe` (4, 2)
    it "3 multipled by 6 is 18" $ do
      multiply 3 6 `shouldBe` (18 :: Integer)
    it "10 multiplied by 0 is 0" $ do
      multiply 10 0 `shouldBe` (0 :: Integer)
    it "7 multiplied by -2 is -14" $ do
      multiply 7 (-2) `shouldBe` (-14 :: Integer)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
               Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- what quickcheck does to get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

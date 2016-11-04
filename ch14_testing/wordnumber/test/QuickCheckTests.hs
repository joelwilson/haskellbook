module QuickCheckTests where

import Data.List (sort)
import Test.QuickCheck
import Test.Hspec
import TestModule (
        halfIdentity,
        listOrdered,
        plusAssociative,
        plusCommutative,
        multAssociative,
        multCommutative,
        quotRemLaw,
        divModLaw,
        expAssociative,
        expCommutative,
        doubleReverseId,
        dollarIdentity,
        foldrConsEqAppend,
        foldrAppendEqConcat,
        exercise10,
        readShowId,
        twice,
        fourTimes,
        capitalizeWord)

runQCTests :: IO ()
runQCTests = hspec $ do
  describe "halfIdentity" $ do
    it "halfIdentity x is equal to x" $ do
      property $ \x -> halfIdentity x == x
  describe "listOrdered" $ do
    it "returns true when passed a list of Integers sorted by the sort function" $ do
      property $ \xs -> listOrdered $ sort (xs :: [Integer])
    it "returns true when passed a list of Chars sorted by the sort function" $ do
      property $ \xs -> listOrdered $ sort (xs :: [Char])
  describe "plusAssociative" $ do
    it "returns true for all Integers" $ do
      property $ \x y z -> (plusAssociative (x :: Integer) (y :: Integer) (z :: Integer))
  describe "plusCommutative" $ do
    it "returns true for all Integers" $ do
      property $ (plusCommutative :: Integer -> Integer -> Bool)
    it "returns true for all Doubles" $ do
      property $ (plusCommutative :: Double -> Double -> Bool)
  describe "multAssociative" $ do
    it "returns true for all Integers" $ do
      property $ (multAssociative :: Integer -> Integer -> Integer -> Bool)
  describe "multCommutative" $ do
    it "returns true for all Integer" $ do
      property $ (multCommutative :: Integer -> Integer -> Bool)
    it "returns true for all Doubles" $ do
      property $ (multCommutative :: Double -> Double -> Bool)
  describe "quotRemLaw" $ do
    it "is true for all Integers" $ do
      property $ (quotRemLaw :: Integer -> Integer -> Bool)
  describe "divModLaw" $ do
    it "is true for all Integers" $ do
      property $ (divModLaw :: Integer -> Integer -> Bool)
  describe "expAssociative" $ do
    it "is true (NOT REALLY) for all Integers" $ do
      property $ (expAssociative :: Integer -> Integer -> Integer -> Bool)
  describe "expCommutative" $ do
    it "is true (NOT REALLY) for all Integers" $ do
      property $ (expCommutative :: Integer -> Integer -> Bool)
  describe "doubleReverseId" $ do
    it "is true for all lists of Integers" $ do
      property $ (doubleReverseId :: [Integer] -> Bool)
  describe "dollarIdentity" $ do
    it "f $ a is equal to f a where f = (*6)" $ do
      property $ ((dollarIdentity (*6)) :: Int -> Bool)
  describe "foldrConsEqAppend " $ do
    it "foldr (:) is equal to (++)" $ do
      property $ (foldrConsEqAppend :: [Int] -> [Int] -> Bool)
  describe "foldrAppendEqConcat" $ do
    it "foldr (++) [] is equal to concat" $ do
      property $ (foldrAppendEqConcat :: [[Int]] -> Bool)
  describe "exercise10" $ do
    it "length of taking n (Int) elements of an array is equal to the value of n" $ do
      property $ (exercise10 :: Int -> [Integer] -> Bool)
  describe "readShowId" $ do
    it "showing than reading a value is equal to the value" $ do
      property $ (readShowId :: String -> Bool)
  describe "idempotence" $ do
    it "word capitalized once is the same as twice" $ do
      property $ (\x -> capitalizeWord (x :: String) == twice capitalizeWord x)
    it "word capitalized twice is same as 4 times" $ do
      property $ (\x -> twice capitalizeWord (x :: String) == fourTimes capitalizeWord x)
    it "word capitalized 4 times is the same as once" $ do
      property $ (\x -> capitalizeWord (x :: String) == fourTimes capitalizeWord x)
    it "sorting once is same result as sorting twice" $ do
      property $ (\x -> sort (x :: [Int]) == twice sort x)
    it "sorting 4 times is same as sorting twice" $ do
      property $ (\x -> fourTimes sort (x :: [Int]) == twice sort x)

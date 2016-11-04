module Main where

import Test.QuickCheck

-- Trivial example
data Trivial =
  Trivial
  deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen =
  return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

main :: IO ()
main = do
  sample trivialGen

-- Identity example
data Identity a =
  Identity a
  deriving (Eq, Show)

identityGen :: Arbitrary a =>
               Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

identityGenChar :: Gen (Identity Char)
identityGenChar = identityGen

-- Products example (Pair)
data Pair a b =
  Pair a b
  deriving (Eq, Show)

pairGen :: (Arbitrary a,
            Arbitrary b) =>
           Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a,
          Arbitrary b) =>
         Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

import Test.QuickCheck
import Data.Semigroup as S

---------------------------------
-- Chapter exercises: Semigroup
---------------------------------
-- Use QC to validate all instances (just associativity).

-- Need this for testing associativity law of Semigroup
semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1. Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  x <> x' = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2. Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

-- Generate arbitrary values of Identity a.
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return $ Identity a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3. Two a b
data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoAssoc =
     Two String String
  -> Two String String
  -> Two String String
  -> Bool

-- 4. Three a b c
data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

type ThreeAssoc =
     Three String String String
  -> Three String String String
  -> Three String String String
  -> Bool

-- 5. data Four a b c d
data Four a b c d = Four a b c d
  deriving (Eq, Show)

-- Semigroup instance
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

-- Gen function to use as the arbitrary function
fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

-- Arbitrary instance
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = fourGen

-- FourAssoc type
type FourAssoc =
     Four String String String String
  -> Four String String String String
  -> Four String String String String
  -> Bool

-- 6. newtype BoolConj
newtype BoolConj = BoolConj Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)

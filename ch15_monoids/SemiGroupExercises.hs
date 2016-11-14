import Test.QuickCheck hiding (Success, Failure)
import Data.Semigroup as S

---------------------------------
-- Chapter exercises: Semigroup
---------------------------------
-- Use QC to validate all instances (just associativity).

-- Need this for testing associativity law
semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- For testing identity law of monoids
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

-- 1. Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  x <> x' = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2. Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance (Num a, Semigroup a) => Monoid (Identity a) where
  -- Ironically, there is no identity (mempty) unless we add
  -- a type constraint, such as `Num a` as I've done here.
  mempty = Identity 0
  mappend = (<>)

-- Generate arbitrary values of Identity a.
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return $ Identity a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

-- This is needed to use Sum in main...maybe this isn't right.
instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = do
    a <- arbitrary
    return $ Sum a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3. Two a b
data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Num a, Num b, Semigroup a, Semigroup b) => Monoid (Two a b) where
  mempty = Two 0 0
  mappend = (<>)

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

instance (Num a, Num b, Num c, Semigroup a, Semigroup b, Semigroup c) =>
  Monoid (Three a b c) where
    mempty = Three 0 0 0
    mappend = (<>)

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

type ThreeId = Three (Sum Integer) (Sum Integer) (Sum Integer) -> Bool

-- 5. data Four a b c d
data Four a b c d = Four a b c d
  deriving (Eq, Show)

-- Semigroup instance
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') =
      Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Num a, Num b, Num c, Num d, Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Monoid (Four a b c d) where
    mempty = Four 0 0 0 0
    mappend = (<>)

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

type FourId = Three (Sum Integer) (Sum Integer) (Sum Integer) -> Bool

-- 6. newtype BoolConj
newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

-- semigroup instance
instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj False) = BoolConj True
  (BoolConj False) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj False
  mappend = (<>)

-- gen func
boolConjGen :: Gen BoolConj
boolConjGen = oneof [
  return $ BoolConj True,
  return $ BoolConj False ]

-- arb instance
instance Arbitrary BoolConj where
  arbitrary = boolConjGen

-- boolConjGen type
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7. newtype BoolDisj
newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

boolDisjGen :: Gen BoolDisj
boolDisjGen = oneof [
  return $ BoolDisj True,
  return $ BoolDisj False ]

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8. data Or
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst _) <> (Fst b) = Fst b
  (Fst _) <> (Snd b) = Snd b
  (Snd a) <> _       = Snd a

instance Num a => Monoid (Or a b) where
  mempty = Fst 0
  mappend = (<>)

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ Fst a,
          return $ Snd b ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

type OrAssoc =
     Or String String
  -> Or String String
  -> Or String String
  -> Bool

-- 9. Combine a b
-- This one is a bit crazy.
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

-- Semigroup instance
-- Returns a new Combine instance containing a  function which applies
-- the argument to both functions and then mappends (<>) the results together.
instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> (f x) <> (g x)

-- TODO: Gen
-- TODO: Arbitrary instance (use CoArbitrary?)
-- TODO: association type

-- 10. Comp a
newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

-- TODO: try to implement and use CoArbitrary

-- 11. Validation a b
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

-- Semigroup only combines failures because this is for validation
-- where it should validate multiple things and combine the failures.
instance Semigroup a =>
  Semigroup (Validation a b) where
    (Failure a) <> (Failure a') = Failure $ a <> a'
    (Failure a) <> (Success _)  = Failure a
    (Success _) <> (Failure a)  = Failure a
    (Success b) <> (Success _)  = Success b

validationGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
validationGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ Success a,
          return $ Failure b ]

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Validation a b) where
    arbitrary = validationGen

type ValidationAssoc =
     Validation String String
  -> Validation String String
  -> Validation String String
  -> Bool

-- 12. AccumulateRight a b
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success b)) <> (AccumulateRight (Success b')) =
    AccumulateRight (Success $ b <> b')
  (AccumulateRight _) <> (AccumulateRight (Failure a)) =
    AccumulateRight $ Failure a
  (AccumulateRight (Failure a)) <> (AccumulateRight _) =
    AccumulateRight $ Failure a

accumRightGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateRight a b)
accumRightGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ AccumulateRight $ Success b,
          return $ AccumulateRight $ Failure a ]

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (AccumulateRight a b) where
    arbitrary = accumRightGen

type AccumRightAssoc =
     AccumulateRight String String
  -> AccumulateRight String String
  -> AccumulateRight String String
  -> Bool

-- 13. AccumulateBoth a b
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

-- TODO: this seems super verbose. Is there a more conscise way?
instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Success b)) <> (AccumulateBoth (Success b')) =
      AccumulateBoth $ Success $ b <> b'
    (AccumulateBoth (Failure a)) <> (AccumulateBoth (Failure a')) =
      AccumulateBoth $ Failure $ a <> a'
    (AccumulateBoth (Success _)) <> (AccumulateBoth (Failure a)) =
      AccumulateBoth $ Failure a
    (AccumulateBoth (Failure a)) <> (AccumulateBoth (Success _)) =
      AccumulateBoth $ Failure a

accumBothGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateBoth a b)
accumBothGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ AccumulateBoth $ Success b,
          return $ AccumulateBoth $ Failure a ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = accumBothGen

type AccumBothAssoc =
     AccumulateBoth String String
  -> AccumulateBoth String String
  -> AccumulateBoth String String
  -> Bool

main :: IO ()
main = do
  putStrLn "Testing Trivial..."
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  putStrLn "\nTesting Identity..."
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Sum Integer -> Bool)
  quickCheck (monoidRightIdentity :: Sum Integer -> Bool)

  putStrLn "\nTesting Two..."
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two (Sum Integer) (Sum Integer) -> Bool)
  quickCheck (monoidRightIdentity :: Two (Sum Integer) (Sum Integer) -> Bool)

  putStrLn "\nTesting Three..."
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentity :: ThreeId)
  quickCheck (monoidRightIdentity :: ThreeId)

  putStrLn "\nTesting Four..."
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: FourId)
  quickCheck (monoidRightIdentity :: FourId)

  putStrLn "\nTesting BoolConj..."
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  putStrLn "\nTesting BoolDisj..."
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  putStrLn "\nTesting Or..."
  quickCheck (semigroupAssoc :: OrAssoc)

  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumRightAssoc)
  quickCheck (semigroupAssoc :: AccumBothAssoc)

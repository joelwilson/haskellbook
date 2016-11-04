import Data.Monoid
import Test.QuickCheck

-- Validating associativity with QuickCheck
monoidAssoc :: (Eq m, Monoid m) =>  m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Quickchecking left and right identity
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Testing QuickCheck's patience


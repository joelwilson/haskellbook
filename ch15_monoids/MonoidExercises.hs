import Data.Monoid
import Test.QuickCheck
import QuickCheckSandbox

-- Exercise : Optional Monoid
-- Write the Monoid instance
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (x <> y)
  mappend Nada x = x
  mappend x Nada = x

-- Exercise; Maybe Another monoid
-- Write a monoid instance which doesn't require a Monoid for the contents.
-- (Reusing the QuickCheck properties from QuickCheckSandbox.hs)
newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' $ Nada
  mappend (First' (Only a)) _ = First' $ Only a
  mappend _ (First' (Only a)) = First' $ Only a
  mappend _ _ = First' $ Nada

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

-- needed to write these two!
firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  oneof [return $ First' Nada,
         return $ First' (Only a)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

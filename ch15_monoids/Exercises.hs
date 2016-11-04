import Data.Monoid

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



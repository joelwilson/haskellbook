{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module TooMany where

-- without GeneralizedNewtypeDeriving:

class TooMany' a where
  tooMany' :: a -> Bool

instance TooMany' Int where
  tooMany' n = n > 42

newtype Goats' = Goats' Int deriving (Eq, Show)

-- this is the same as the Int instance,
-- but we have to define it separately

instance TooMany' Goats' where
  tooMany' (Goats' n) = tooMany' n

-- with GeneralizedNewtypeDeriving:

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- now we can just derive TooMany because
-- Int is already an instance of TooMany
-- and newtype *is* an Int at runtime anyway.
newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

-- exercises
-- 1  write a TooMany instance of type (Int, String).
--    Requires FlexibleInstances if newtype isn't used.

-- with newtype:
newtype IntString = IntString (Int, String) deriving (Eq, Show)

instance TooMany IntString where
  tooMany (IntString (n, _)) = n > 42

-- without newtype (with FlexibleInstances):
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- 2. write TooMany instance for (Int, Int), summing the values.
instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42

-- 3. write TooMany instance for (Num a, TooMany a) => (a, a)
-- This doesn't work if the Int instance is defined
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x && tooMany y

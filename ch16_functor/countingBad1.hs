-- explores the composition law of functors
data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

-- super NOT okay
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

-- This definition breaks the composition law because
-- every time a function is applied via fmap, n is
-- incremented.
--
-- Here, fmap is only applied once to the functor, so n is
-- incremented only once:
-- > fmap ((++ " blah") . (++ " wut")) $ Heisenberg 0 "Izzy"
-- Heisenberg 1 "Izzy wut blah"
--
-- But here, fmap is applied twice, so n is incremented two times!
-- > (fmap (++ " blah")) . (fmap (++ " wut")) $ Heisenberg 0 "Izzy"
-- Heisenberg 2 "izzy wut blah"

data CountingGood a =
  Heisenberg' Int a
  deriving (Eq, Show)

-- Totes cool.
instance Functor CountingGood where
  fmap f (Heisenberg' n a) = Heisenberg' n (f a)

-- n is no longer screwed with, so it's cool.
-- It seems like there can only be "changes" to a functor
-- if the function used to fmap does the modification. I wonder
-- if that holds up...

-- ACTUALLY, f can ONLY be applied to `a` because the functor type class
-- is on `CountingGood a`, so how could it possibly apply to `n` as well
-- (an int) unless we really super constrain the type class.

-- Given a datatype declaration, what can we do?

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah

-- Which of the following will typecheck? For those that don't, why?

-- 1
-- phew = Papu "chases" True
-- Does not typecheck. String /= the Rocks data constructor. Bool /= the
-- the Yeah data constructor. This works:
phew = Papu (Rocks "chases") (Yeah True)

-- 2
truth = Papu (Rocks "chomskydoz")
             (Yeah True)
-- (It does typecheck)

-- 3
-- equalityForAll :: Papu -> Papu -> Bool
-- equalityForAll p p' = p == p'
-- Does not typecheck b/c Papu does not implement Eq.

-- 4
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
-- Nope. Papu doesn't implement Ord. Or anything for that matter.

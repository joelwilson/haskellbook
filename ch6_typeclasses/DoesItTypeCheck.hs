-- 1
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2
data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                 then Blah
                 else x

-- 3 If you were able to get settleDown to typecheck:
-- a) What values are acceptable inputs to that function?
--    Any Mood
-- b) What will happen if you try to run settleDown 9? Why?
--    A compiler error will result saying something about a type
--    mistmatch because 9 is not a data constructor of Mood.
-- c) What will happen if you try to run Blah > Woot? Why?
--    It won't work because Mood does not derive Ord.

-- 4
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "julie" "loves" "dogs"


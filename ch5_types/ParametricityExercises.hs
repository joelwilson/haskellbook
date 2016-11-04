-- 1. Given the type a->a (id) attempt to make a function
-- that is not bottom and terminates successfully that
-- does something other than returning the same value.
-- this is impossible but you should try anyway.
aFunc :: a -> a
aFunc x = x
-- NOPE: aFunc a = a + 3

-- 2. We can get more comfortable appreciation of parametricity
-- by looking at a -> a -> a. This hypothetical function has two
-- and only two implementations. Write both possible versions.
-- After doing so, try to violate the constraints of
-- parametrically polymorphic values we outlined above.
aaFunc1 :: a -> a -> a
aaFunc1 x y = x

aaFunc2 :: a -> a -> a
aaFunc2 x y = y

-- 3. Implement a -> b -> b.
abFunc :: a -> b -> b
abFunc x y = y
-- How many implementations can it have?
--   1
-- Does the behavior change when the types of a and b change?
--   No. If a changes, it can still only return b. Same if b
--   changes. It is like a demented id function.

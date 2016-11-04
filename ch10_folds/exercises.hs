-- 1 stop vowel stop
stops  = "pbtdkg"
vowels = "aeiou"

-- a) all combos of stop letter stop words
threeTupleCombos xs ys = [(x, y, x') | x <- xs, x' <- xs, y <- ys]

stopVowelStop = threeTupleCombos stops vowels

-- b) exclude words starting with p
stopVowelStopLimited = filter (\(x, _, _) -> x /= 'p') stopVowelStop

-- bonus (personal exploration)
threeLetterWords = map combine stopVowelStop

threeLetterWordsLimited = map combine stopVowelStopLimited

combine (s, v, s') = [s, v, s']

-- c) same but for nouns and verbs
nouns = ["cat", "human", "poop", "chair", "octopus"]
verbs = ["run", "hike", "bike", "swim"]

nounVerbNoun = threeTupleCombos nouns verbs

-- 2 What does this do? What is its type?
seekritFunc x =
  div (sum (map length' (words x)))
      (length' (words x))

length' :: (Foldable t, Num b) => t a -> b
length' = fromIntegral . length

-- It gets the average number of characters in the words in a string.

-- It's type:
seekritFunc :: Integral a => String -> a

-- 3 rewrite seekritFunc using fractional division
preciseSeekritFunc :: Fractional a => String -> a
preciseSeekritFunc x =
  (/) (sum (map length' $ words x))
      (length' $ words x)

--------------
-- Rewriting functions using folds (point-free if you can)
--------------

-- 1 myOr
myOr :: [Bool] -> Bool
myOr = foldr (\x acc -> if x then x else acc) False

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

-- 2 myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny = go
  where go f = foldr (\x acc -> f x || acc) False

-- 3 myElem
myElem :: Eq a => a -> [a] -> Bool
myElem = go
  where go x = foldr (\y acc -> y == x || acc) False

-- 4 myReverse
myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

-- 5 myMap
myMap :: (a -> b) -> [a] -> [b]
myMap = go
  where go f = foldr (\x acc -> f x : acc) []

-- 6 myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = go
  where go f = foldr (\x acc -> if f x then x : acc else acc) []

-- 7 squish
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8 squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f

-- 9 squishAgain (re-using the squishMap function)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10 myMaximumBy returns the greatest element of the list
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = go
  where go f xs = foldr (\x acc -> if f x acc == GT then x else acc)
                        (head xs)
                        (tail xs)

-- myMaximumBy (\_ _ -> LT) [1..10]

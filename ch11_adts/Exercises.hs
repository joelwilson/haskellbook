import Data.Char

-- 1. given:
data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

-- a) Weekday is a type with five data constructors
-- True
-- b) Weekday is a tree with five branches
-- False. It has no branches or leaves.
-- c) Weekday is a product type
-- False. It is a sum type.
-- d) Weekday takes five arguments
-- False. It takes no type arguments.

-- 2. what is the type of the following function?
f Friday = "Miller Time"
-- c) f :: Weekday -> String

-- 3. Types defined with the data keyword
-- b (must begin with capital letter)

-- 4. The function g xs = xs !! (length xs -1)
-- (c) delivers the final element of xs

----------------------------
-- Exercise: Vigenere cipher
----------------------------
vigenere :: String -> String -> String
vigenere s k = zipWith vigenere' s (concat [k | r <- [1..r']])
  where r' = 1 + (length s `div` length k)
        vigenere' :: Char -> Char -> Char
        vigenere' x y
          | x == ' '  = ' '
          | otherwise = addOffset x $ chToOffset y

addOffset :: Char -> Int -> Char
addOffset c n = chr $ n' + base
  where n'   = mod ((ord . toUpper $ c) + n - ord 'A') 26
        base = if isUpper c then ord 'A' else ord 'a'

chToOffset :: Char -> Int
chToOffset c = c' - ord 'A'
  where c' = ord . toUpper $ c

-------------------------
-- Exercises: As-patterns
-------------------------

-- 1. should return True if all the values in the first
-- list appear in the second list. No need to be contigous.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf (x:xs) ys = (x `elem` ys) && isSubsequenceOf xs ys
isSubsequenceOf [] ys = True
-- Why would as-patterns be useful here???

-- 2. Split a sentence into words, then tuple each word
-- with the capitalized form of each.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = (map pairWithCapitalized) . words
  where pairWithCapitalized w@(x:xs) = (w, toUpper x : xs)

------------------------
-- Language exercises
------------------------

-- 1. write a function that capitalizes a word
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

-- 2. capitalize the sentences in a paragraph.
capitalizeParagraph :: String -> String
capitalizeParagraph =
  (concatMap capitalizeWord) . sentences

sentences :: String -> [String]
sentences [] = []
sentences x  = let (sent, rest) = takeSentence x ""
               in sent : sentences rest

takeSentence :: String -> String -> (String, String)
takeSentence (x:[]) s    = (s ++ [x], "")
takeSentence (x:y:xs) s
  | x == '.' && y == ' ' = (s ++ [x] ++ " ", xs)
  | x == '.'             = (s ++ [x], y:xs)
  | otherwise = takeSentence (y:xs) (s ++ [x])

------------------
-- Phone exercise
------------------

-- 1. Create a data structure that captures the phone layout.

data DaPhone = DaPhone [(Char, String)]
-- meh...TODO: come back to this problem later

------------------
-- Hutton's Razor
------------------

-- 1. write 'eva' which reduces an expression to a final sum
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add x y) = eval x + eval y

-- 2. Write a printer for the expressions
printExpr :: Expr -> String
printExpr (Lit i)   = show i
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y

-- String processing
import Data.Char (toLower)
import Data.Maybe (isJust)

-- 1. write a recursive function which replaces "the" with "a".
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe w = Just w

replaceThe :: String -> String
replaceThe s = removeLast $ concatMap (++ " ") $ go (notThe w) ws
  where (w:ws) = words s
        go :: Maybe String -> [String] -> [String]
        go (Just x) [] = [x]
        go Nothing  (y:ys) = "a" : go (notThe y) ys
        go (Just x) (y:ys) = x   : go (notThe y) ys

removeLast :: String -> String
removeLast x = fst $ splitAt (length x - 1) x

-- 2. count the number of "the" followed by a vowel
isVowel :: Char -> Bool
isVowel c = toLower c `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where go []       = 0
        go (x:[])   = 0
        go (x:y:[]) = 0
        go (x:y:xs)
          | isJust $ notThe x = go xs
          | isVowel $ head y     = 1 + go xs

-- 3. if num of vowels exceeds the num of consonants, return Nothing
--    else return Just the string.
newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord w = if countVowels w > halfLength
           then Nothing
           else Just $ Word' w
  where countVowels = foldr (\x n -> if isVowel x then n+1 else n) 0
        halfLength = (fromIntegral $ length w) / 2



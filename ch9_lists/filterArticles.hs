import Data.Char

toUpperString :: String -> String
toUpperString xs = [toUpper x | x <- xs]

isArticle :: String -> Bool
isArticle x = x `elem` ["the", "a", "an"]

myFilter :: String -> [String]
myFilter xs = filter (not . isArticle) $ words xs

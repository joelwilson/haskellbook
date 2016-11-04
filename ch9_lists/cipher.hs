module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar n = map $ shift n

uncaesar :: Int -> String -> String
uncaesar n = caesar (negate n)

shift :: Int -> Char -> Char
shift n c
  | isUpper c = shift' 'A'
  | isLower c = shift' 'a'
  where shift' b = chr $ (ord c - ord b + n) `mod` 26 + (ord b)


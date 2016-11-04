module PoemLines where

myWords :: String -> [String]
myWords = splitAllOn (' ')

splitAllOn :: Char -> String -> [String]
splitAllOn c [] = []
splitAllOn c x  = first : splitAllOn c rest
  where
    first = dropWhile (== c) . takeWhile (/= c) $ x
    rest  = dropWhile (== c) . dropWhile (/= c) $ x

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

-- purStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame they fearful symmetry?

myLines :: String -> [String]
myLines = splitAllOn '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

-- small test
main :: IO ()
main =
  print $ "Are they equal? "
          ++ show (myLines sentences == shouldEqual)

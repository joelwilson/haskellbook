module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- Exercise 5 - pointfree version of roundTrip
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

-- Exercise 6 -- new signature
roundTripDiffTypes :: (Show a, Read b) => a -> b
roundTripDiffTypes = read . show

main :: IO ()
main = do
  print (roundTrip 4)
  print (roundTripPF 4)
  print ((roundTripDiffTypes 4) :: Int) -- ex 6
  print (id 4)

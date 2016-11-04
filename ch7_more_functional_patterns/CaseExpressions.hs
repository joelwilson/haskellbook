funcZ :: (Num a, Eq a) => a -> String
funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal :: (Eq a) => [a] -> String
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' :: (Eq a) => [a] -> String
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

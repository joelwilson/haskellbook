
-- 3
thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- 4
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

-- 5
rvrs :: String
rvrs = drop 9 x ++ " " ++ (take 2 (drop 6 x)) ++ " " ++ take 5 x
  where x = "Curry is awesome"


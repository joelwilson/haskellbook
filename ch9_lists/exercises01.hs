myWords :: String -> [String]
myWords [] = []
myWords x  = word : myWords rest
  where
    word = dropWhile (== ' ') . takeWhile (/= ' ') $ x
    rest = dropWhile (== ' ') . dropWhile (/= ' ') $ x

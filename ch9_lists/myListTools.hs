myTail        :: [a] -> [a]
myTail []     = []
myTail (_:xs) =  xs

safeTail        :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

myHead       :: [a] -> a
myHead (x:_) = x

safeHead       :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

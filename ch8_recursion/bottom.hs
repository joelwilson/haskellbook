f :: Bool -> Int
f True = error "blah"
f False = 0

f' :: Bool ->  Int
f' False = 0

data Maybe a = Nothing | Just a

g :: Bool -> Maybe Int
g False = Just 0
g _ = Nothing



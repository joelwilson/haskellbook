myZip :: [a] -> [b] -> [(a, b)]
myZip [] ys = []
myZip xs [] = []
myZip xs ys = (head xs, head ys) : myZip (tail xs) (tail ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] ys = []
myZipWith f xs [] = []
myZipWith f xs ys = f (head xs) (head ys)
                    : myZipWith f (tail xs) (tail ys)

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWith (\x y -> (x, y))

fibs    = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

fibsFirst x = take x fibs

fibsUnder1000 = takeWhile (<1000) fibs

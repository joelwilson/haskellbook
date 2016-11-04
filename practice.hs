mult1     = x * y
  where x = 5
        y = 6

letTest = let y = 8
          in y + 3

whereThousand x = x * y
  where y = 1000

threeTimesThreePlusOneThousand = x * 3 + y
  where x = 3
        y = 1000

xTimesFive = x * 5
  where y = 10
        x = 10 * 5 + y

zDiv7PlusNegx = z / x + y
  where x = 7
        y = negate x
        z = y * 10


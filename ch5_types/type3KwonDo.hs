data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xForm :: (X, Y) -> (Z, Z)
xForm (x, y) = (xz x, yz y)

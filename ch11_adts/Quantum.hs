data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

-- 3 + 3 (because Either is a sum type)
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

-- you do the rest
quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- 3 * 3 (because (,) is a product type)
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)
-- you do the rest

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)

-- 3 ^ 3 (function is exponentiation)
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes  = Yes
quantFlip3 No   = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes  = Yes
quantFlip4 No   = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes  = Yes
quantFlip5 No   = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes  = No
quantFlip6 No   = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes  = Both
quantFlip7 No   = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes  = Both
quantFlip8 No   = Yes
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes  = Both
quantFlip9 No   = No
quantFlip9 Both = No

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes  = Both
quantFlip10 No   = No
quantFlip10 Both = Both


-- there should be 2^3 (8) implementations of this function:
convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes  = True
convert4 No   = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = True
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = False
convert8 Both = True

-- Exercises : The Quad.
-- Determine how many unique inhabitants each type has.

-- 1.
data Quad' =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

eQuad :: Either Quad Quad
eQuad = undefined

-- Either is a sum type (+)
-- Quad has 4 unique inhabitants.
-- 4 + 4 = 8

-- 2.
prodQuad :: (Quad, Quad)
-- 4 * 4 = 16

-- 3.
funcQuad :: Quad -> Quad
-- 4 ^ 4 = 256

-- 4.
prodTBool :: (Bool, Bool, Bool)
-- 2 * 2 * 2 = 8

ptb1 = (True,  True,  True)
ptb2 = (False, True,  True)
ptb3 = (False, False, True)
ptb4 = (False, False, False)
ptb5 = (False, True,  False)
ptb6 = (True,  True,  False)
ptb7 = (True,  False, False)
ptb8 = (True,  False, True)

-- 5.
gTwo :: Bool -> Bool-> Bool
-- 2 ^ 2 ^ 2 = 4 ^ 2 = 16

-- 6.
fTwo :: Bool -> Quad -> Quad
--      a    -> b    -> c
--      2    -> 4    -> 4
-- (4 ^ 4) ^ 2
-- 4 ^ 8
-- 65536

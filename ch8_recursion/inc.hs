incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)
-- Exerxise: write out the evaluation of:
-- applyTimes 5 (+1) 5
--    (+1) (applyTimes (5-1) (+1) 5)
--    (+1) ((+1) (applyTimes (4-1) (+1) 5))
--    (+1) ((+1) ((+1) (applyTimes (3-1) (+1) 5)))
--    (+1) ((+1) ((+1) ((+1) (applyTimes (2-1) (+1) 5))))
--    (+1) ((+1) ((+1) ((+1) ((+1) applyTimes (1-1) (+1) 5))))
--    (+1) ((+1) ((+1) ((+1) ((+1) 5))))
--    (+1) ((+1) ((+1) ((+1) (6))))
--    (+1) ((+1) ((+1) 7))
--    (+1) ((+1) 8)
--    (+1) (9)
--    10

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

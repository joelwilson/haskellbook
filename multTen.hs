multTen :: (Integral n, Eq n) => n -> Maybe n
multTen n = if n == 0 then Nothing else (Just $ n `div` 10)

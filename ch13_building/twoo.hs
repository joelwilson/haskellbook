twoo :: IO Bool
twoo = do c  <- getChar
          c' <- getChar
          return (c == c')

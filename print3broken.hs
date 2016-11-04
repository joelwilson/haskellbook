-- print3broken.hs
module Print3Broken where

printSecond :: String -> IO ()
printSecond greet = do
  putStrLn greet

main :: IO ()
main = do
  putStrLn greeting
  printSecond greeting
    where greeting = "Yarrrrr"

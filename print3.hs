-- print3.hs
module Print3 where

myGreeting :: String
-- the above line reads as: "myGreeting has the type String"
myGreeting = "Hello" ++ " world"
-- could also be: "hello" ++ " " ++ "world!"
-- to obtain the same result

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = concat [hello, " ", world]

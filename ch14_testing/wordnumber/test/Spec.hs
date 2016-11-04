module Main where

import WordNumberTest
import QuickCheckTests

main :: IO ()
main = do
  runWordTests
  runQCTests

module Main where

import System.Environment (getArgs)
import JsonParser (parseJsonC)


main :: IO ()
main = do
  args <- getArgs
  let file = head args

  content <- readFile file
  let json = parseJsonC content
  print json

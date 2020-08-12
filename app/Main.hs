module Main where

import JsonParser (parseJsonC)


main :: IO ()
main = do
  content <- getContents
  let json = parseJsonC content
  print json

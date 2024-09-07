module Main (main) where

import System.Environment
import Parser (parseExamples, parseShow)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x:_) -> do
      s <- readFile x
      parseShow x s
    _ -> parseExamples

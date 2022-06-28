module Main (main) where

import InterpreterBase (analyze, prog10)

main :: IO ()
main = do
  analyze prog10

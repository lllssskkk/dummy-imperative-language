module Main (main) where

import Analyzer (analyze)
import InterpreterBase (prog10)

main :: IO ()
main = do
    analyze prog10

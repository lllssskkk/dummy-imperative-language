module Main (main) where

import Analyzer (analyze)
import Interpreter (prog10)

main :: IO ()
main = do
    analyze prog10

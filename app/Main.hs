module Main (main) where

import Analyzer (analyze)
import Constant (lambdaSymbol)
import Interpreter (run)
import Program

main :: IO ()
main = do
    -- putStrLn $ lambdaSymbol ++ "Start analyzing!"
    -- analyze prog10
    putStrLn $ lambdaSymbol ++ "Start Executing!"
    run prog10

prog10 :: Program
prog10 = do
    -- "scratch" .= var "arg"
    -- "scratch1" .= var "scratch2"
    "unusedVar1" .= bool True
    "unusedVar2" .= bool True
    function "test" ["x"] (Not (Var "x"))
    call "test" [B True]
    function "initVal" ["x", "y"] (Add (Var "x") (Var "y"))
    call "initVal" [I 1, I 2]
    call "initVal" [I 10, I 20]
    "arg" .= int 10
    "counter" .= var "arg"
    "total" .= int 1
    while (var "counter" `Gt` int 1)
        ( do
            "total" .= ("total" :: String) .* ("counter" :: String)
            "counter" .= ("counter" :: String) .- (1 :: Int)
            -- breakpoint
            printt (var "counter")
        )
    printt $ var "total"
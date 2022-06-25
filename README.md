
<del>
There are 2 files created for this project, base and base2.

base2 should solve problems 2, 3 and 5 (uninitialized variable error and unused variable warning). It doesn't support conditional break

base is a fork of base2 and enhenced with conditional break. 

ghci should work fine with both files.
----------------------------------------------------------------------
prog10 :: Program
prog10 = do  
           --"scratch" .=  (var "arg", "B")
           "unusedVar1" .= (bool True, "Continue")
           "unusedVar2" .= (bool True, "Continue")
           "arg"     .= (int 10, "Break")
           "scratch" .=  (var "arg", "Break")
           "total"   .= (int 1, "Break")
           while "Continue" ( (var "scratch") `Gt` (int 1) ) (
            do "total"   .=  ("total" .* "scratch", "Continue")
               "scratch" .=  ("scratch" .- (1::Int) , "Continue")
               print "Continue" $ (var "scratch") 
            ) 
           print "Continue" $ (var "total") 
-----------------------------------------------------------------------
ghci > :load InterpreterBase2


Base2 part 2 and 3 

The difference between the interpreter7 original file and Base2 is that all functions take an additional argument. The additional parameter is a directive that instructs the interpreter how to behave when it reaches the line of code.
If the instruction is a "Continue", the interpreter will continue to execute until a non-continue instruction is encountered. This means that "Break" is intended solely for the sake of memorizing and comparison. Anything other than Continue serves as a signal to the interpreter to halt.

When a non-continue instruction is encountered, the user has the choice to type any of the following three : Continue / Break/ a Variable name.

Continue will tell the interpreter to execute until a next non-continue instruction is encountered

Break basically does nothing, it servers the functionality as the f5 in windows

If a Variable name is typed, the interpreter will output the variable value to the console. If the interpreter cannot find the variable in the state, a error will pop out

Example :

*Main> run prog10
Continue
Continue
Continue
I 9
I 8
I 7
I 6
I 5
I 4
I 3
I 2
I 1
I 3628800
fromList [("arg",I 10),("scratch",I 1),("total",I 3628800),("unusedVar1",B True),("unusedVar2",B True)]

-------------------------------------------------------------------

static analysis part

With first scratch commented out

*Main> analyze prog10
fromList [("unusedVar1",0),("unusedVar2",0)]

With first scratch

*Main> analyze prog10
"Uncaught exception: Access variable before initialization : arg"

Since scratch is assigned with the value inside of arg and arg is not existed, it pops an exception about arg.


------------------------------------------------------------------------
Base 

ghci > :load InterpreterBase

prog10 :: Program
prog10 = do  
           "scratch" .=  (var "arg", "B")
           conditionalB "Continue" ((int 1) `Gt` (int 1))
           "unusedVar1" .= (bool True, "Break")
           "unusedVar2" .= (bool True, "Continue")
           "arg"     .= (int 10, "Continue")
           "scratch" .=  (var "arg", "Continue")
           "total"   .= (int 1, "Continue")
           while "Continue" ( (var "scratch") `Gt` (int 1) ) (
            do "total"   .=  ("total" .* "scratch", "Continue")
               "scratch" .=  ("scratch" .- (1::Int) , "Continue")
               print "Continue" $ (var "scratch") 

            ) 
           print "Continue" $ (var "total") 

In base, there is a conditional break statement added to the statement data type. The CB takes a instruction argument like other statements. It also takes an expression as input. 


run :: Program -> IO ()
run program = do result <- runExceptT $ (runStateT $ step (snd $ runIdentity $ (runWriterT program)) (Eq (Const $ I 1) (Const $ I 1))  ) Map.empty
                 case result of
                      Right ( (), env ) -> (System.print env)
                      Left exn -> System.print ("Uncaught exception: "++exn)

(Eq (Const $ I 1) (Const $ I 1)) is a dummy expression which is also true after evaluation. If one of the statements of the program is a conditional break statement, the original dummy expression will be replaced by the expression listed in the conditional break statement.
All statements after the conditional break statement will be tested by the expression. If the expression returns false, it servers as a break instruction. If the expression returns true, the interpreter will also check the instruction to decide whether it should execute the current statement.

Unlike base2, base doesn't support the continue instruction in the interactive part. It supports another insruction called "ForceContinue" which bascially perform the same functionality as "Continue" in different way. 

*InterpreterBase> run prog10
ForceContinue
ForceContinue
ForceContinue
ForceContinue
ForceContinue
ForceContinue
ForceContinue
ForceContinue
ForceContinue
I 9
ForceContinue
ForceContinue
ForceContinue
ForceContinue
I 8
ForceContinue
ForceContinue
^CInterrupted.

-------------------------------------------------------------------

base static analysis part

The interpreter could also perform static analysis on the program  equipped with conditional break

*InterpreterBase> analyze prog10
["unusedVar1","unusedVar2"]
*InterpreterBase> :r
[1 of 1] Compiling InterpreterBase  ( InterpreterBase.hs, interpreted )
Ok, one module loaded.
*InterpreterBase> analyze prog10
"Uncaught exception: Access variable before initialization : arg"

</del>
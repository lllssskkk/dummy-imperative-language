In this project you will write a programming language interpreter for an imperative language, based on the interpreter we developed as an example in the Monad Transformers lectures this year. You should start with the version of the interpreter attached to this page -- this one is the same as the Interpreter8.lhs file from the Monad Transformers lecture during the year, but I have removed the "run" function (and all the stuff related to syntactic sugaring). You may use this as a starting point

The project has several parts; you should complete as many as you can.  

1. Turn the program into a 'stack' project so that it's easy for me to build and run it when you submit. Add a Readme file (it can be plain text or markdown formatted, your choice). When you submit the project this file should 
      1. describe how to use the final interpreter, 
   1.  and contain a section for each of the numbered sections of the project describing how much of the section you have completed. (5 marks). 


2. Modify the interpreter so that the Run monad has a step action (instead of an exec action - you can remove exec entirely if you want). step should execute a single statement (in a compound statement such as a loop it should execute only one statement of the body at a time). The intention is that the user will be able to execute the program a statement at a time in order to debug the program. You should also modify the run function so that it uses step to execute the program.  (15 marks). 


3. Modify run so that at each step instead of executing the next instruction it prompts the user interactively (you can give the choice by having the user type in commands, or present a menu of options, the UI of the interpreter is up to you). The users choices should be to: (5 marks). 
   1. execute the next statement, or
   2. print the values of a variable they choose
   
4. Next, extend the interpreter to allow conditional breakpoints. The user should be able to enter a Boolean expression in the programming language which will be tested after each statement. If the condition is true (or invalid -- e.g. if a variable is referenced that doesn't exist in the program yet) then the interpreter should move on to the next statement. If it is false then the program should stop and allow the user to inspect the values of the variables in the program. (10 marks).
   
5.  Finally, add some static analysis to the interpreter to report on errors before the program is run. Add two of the following: (15 marks). 
    1. Add a check for uninitialised variable references, and report to the user when the analyser spots a code path in the program that might be reading from a variable before it is written to
    2. Have the interpreter check for unreachable code (i.e. an "else" that could never be reached because the corresponding condition is a tautology)
    3. Have the interpreter check for unused variables (variables which have been assigned to but are never read)
    4. 
As always, please ensure that your program has suitable comments so that I can follow your design decisions (to get full marks in each section the relevant code has to be both correct and well written)
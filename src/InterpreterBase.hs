{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}

module InterpreterBase where

--I want these language extensions for my syntactic sugaring tricks at the end

--I want my own definition of lookup and I want to write my own function named "print".

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

--I want to get at the standard "print" function using the name System.print

import qualified System.IO as System

--I plan to use these monads to construct the parts of my interpreter

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (
  MonadWriter (tell),
  Writer,
  WriterT (runWriterT),
 )

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

-- data Val = I Int | B Bool
--   deriving (Eq, Show)

data Val where
  I :: Int -> Val
  B :: Bool -> Val
  deriving (Eq, Show)

data Expr where
  Const :: Val -> Expr
  Add :: Expr -> Expr -> Expr
  Sub :: Expr -> Expr -> Expr
  Mul :: Expr -> Expr -> Expr
  Div :: Expr -> Expr -> Expr
  And :: Expr -> Expr -> Expr
  Or :: Expr -> Expr -> Expr
  Not :: Expr -> Expr
  Eq :: Expr -> Expr -> Expr
  Gt :: Expr -> Expr -> Expr
  Lt :: Expr -> Expr -> Expr
  Var :: String -> Expr
  deriving (Eq, Show)

type Name = String
type Env = Map.Map Name Val

lookup :: MonadError String m => String -> Map.Map String a -> m a
lookup k t = case Map.lookup k t of
  Just x -> return x
  Nothing -> throwError ("Unknown variable " ++ k)

{-- Monadic style expression evaluator,
 -- with error handling and Reader monad instance to carry dictionary
 --}

type Eval a = ReaderT Env (ExceptT String Identity) a

runEval :: r -> ReaderT r (ExceptT e Identity) a -> Either e a
runEval env ex = runIdentity (runExceptT (runReaderT ex env))

--This evaluator could be a little neater

--Integer typed expressions
evali :: (Int -> Int -> Int) -> Expr -> Expr -> ReaderT Env (ExceptT String Identity) Val
evali op e0 e1 = do
  e0' <- eval e0
  e1' <- eval e1
  case (e0', e1') of
    (I i0, I i1) -> return $ I (i0 `op` i1)
    _ -> throwError "type error in arithmetic expression"

--Boolean typed expressions
evalb :: (Bool -> Bool -> Bool) -> Expr -> Expr -> ReaderT Env (ExceptT String Identity) Val
evalb op e0 e1 = do
  e0' <- eval e0
  e1' <- eval e1
  case (e0', e1') of
    (B i0, B i1) -> return $ B (i0 `op` i1)
    _ -> throwError "type error in boolean expression"

--Operations over integers which produce booleans
evalib :: (Int -> Int -> Bool) -> Expr -> Expr -> ReaderT Env (ExceptT String Identity) Val
evalib op e0 e1 = do
  e0' <- eval e0
  e1' <- eval e1
  case (e0', e1') of
    (I i0, I i1) -> return $ B (i0 `op` i1)
    _ -> throwError "type error in arithmetic expression"

--Evaluate an expression
eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = evali (+) e0 e1
eval (Sub e0 e1) = evali (-) e0 e1
eval (Mul e0 e1) = evali (*) e0 e1
eval (Div e0 e1) = evali div e0 e1
eval (And e0 e1) = evalb (&&) e0 e1
eval (Or e0 e1) = evalb (||) e0 e1
eval (Not e0) = evalb (const not) e0 (Const (B True))
--  where
--   not2 a _ = not a -- hack, hack
eval (Eq e0 e1) = evalib (==) e0 e1
eval (Gt e0 e1) = evalib (>) e0 e1
eval (Lt e0 e1) = evalib (<) e0 e1
eval (Var s) = do
  env <- ask
  lookup s env

{-------------------------------------------------------------------}
{- The statement language                                          -}

-- In adittional to the original statement, the modifiled version has an instruction string attached to it.
-- The instruction instructs how the statement is perfromed.
-- "Continue" leads to execution until the next Break
-- "Variable name" prints out the the variable value
-- "Break" invokes the same statement with a break instruction attached. It literally does nothing, revisit the same state over again.
-- If the variable name is not existed in the state, it leads to a variable unknown error.

data Statement
  = Assign String Expr String
  | If Expr Statement Statement String
  | While Expr Statement String
  | Print Expr String
  | Seq Statement Statement
  | Try Statement Statement String
  | Pass
  | CB Expr String
  deriving (Eq, Show)

type SaEnv = (Map.Map String Int, [String])

type Sa a = StateT SaEnv (ExceptT String IO) a

runSa :: StateT Env (ExceptT String IO) a -> IO (Either String (a, Env))
runSa p = runExceptT (runStateT p Map.empty)

increRefCount :: (Name, Int) -> Sa ()
increRefCount (s, i) = state (\(table, list) -> ((), (Map.insert s i table, list)))

refPriorInitialization :: String -> Sa ()
refPriorInitialization varName = state (\(table, list) -> ((), (table, varName : list)))

printout' :: String -> Sa ()
printout' = liftIO . System.print

-- parser simplies an expression to a point that only a variable with its name exists
-- parser ignores any types of Var data
-- if a variable is found, parser function first checks whether the variable is already existed in the state environment
-- if true, increase the reference count of the variable by 1
-- if false, we found an Access variable before initialization error
parser :: Expr -> Sa ()
parser (Const _) = return ()
parser (Add e0 e1) = parseri e0 e1
parser (Sub e0 e1) = parseri e0 e1
parser (Mul e0 e1) = parseri e0 e1
parser (Div e0 e1) = parseri e0 e1
parser (And e0 e1) = parseri e0 e1
parser (Or e0 e1) = parseri e0 e1
parser (Not e0) = parser e0
parser (Eq e0 e1) = parserib e0 e1
parser (Gt e0 e1) = parserib e0 e1
parser (Lt e0 e1) = parserib e0 e1
parser (Var s) = do
  (env, _) <- get
  let varReferenceNum = Map.lookup s env
  if isJust varReferenceNum then (do increRefCount (s, 1 + fromJust varReferenceNum)) else (do refPriorInitialization s)
  return ()

parseri :: Expr -> Expr -> StateT SaEnv (ExceptT String IO) ()
parseri e0 e1 = parser e0 >> parser e1

-- --Boolean typed expressions
parserb :: Expr -> Expr -> StateT SaEnv (ExceptT String IO) ()
parserb e0 e1 = parser e0 >> parser e1

-- --Operations over integers which produce booleans

parserib :: Expr -> Expr -> StateT SaEnv (ExceptT String IO) ()
parserib e0 e1 = parser e0 >> parser e1

-- Check is the main body performs the static analysis functionality
-- Check responds to statements correspondingly
--
check :: Statement -> Sa ()
check (CB _ _) = return ()
check (Seq (CB _ _) s1) = check s1
check (Seq s0 s1) = check s0 >> check s1
check (Assign s v _) = do
  parser v
  increRefCount (s, 0)
check (Print e _) = do
  parser e
  return ()
check (If cond s0 s1 _) = do
  parser cond
  check s0
  check s1
  return ()
check (While cond s _) = do
  parser cond
  check s
check (Try s0 s1 _) = catchError (check s0) (\e -> check s1)
check Pass = return ()

-- analyze is the run function of the static analysis part
-- analyze would print a list of unused variable name, if the program has no accessing error.
analyze :: Program -> IO ()
analyze program = do
  result <- runExceptT $ (runStateT $ check $ snd $ runIdentity (runWriterT program)) (Map.empty, [])
  case result of
    Right ((), (env, list)) -> do
      let unused = (++ " are unused variables") $ show . Map.keys $ Map.filter (== 0) env
      let refPriorInit = (++ " got referenced before initialization") $ show list
      System.putStrLn unused
      System.putStrLn refPriorInit
    Left exn -> System.print ("Uncaught exception: " ++ exn)

type Run a = StateT Env (ExceptT String IO) a
runRun p = runExceptT (runStateT p Map.empty)

set :: (Name, Val) -> Run ()
set (s, i) = state (\table -> ((), Map.insert s i table))

-- Before executing a statement, the step function first checks the instruction argument attached to the statement
-- If the instruction argument is "Continue", the step function perfroms the same operation as its counterpart in exec function
-- If not, the step function could accepts one of three instruction, Continue, Break and a "Variable name"
-- With conditional Break enhenced step action
-- step action takes an extra Expr argument as input.
-- The original Expr argument should be a dummy expression which equals to True
-- If one of the statements of the program is a conditional break statement, the original dummy expression will be replaced by the expression listed in the conditional break statement.
step :: Statement -> Expr -> Run ()
step (Assign s v instruction) expre =
  do
    st <- get
    case runEval st (eval expre) of
      Right (B val) -> do
        if val && instruction == "Continue"
          then
            ( case runEval st (eval v) of
                Right val -> set (s, val)
                Left err -> throwError err
            )
          else
            ( do
                instruction <- getInput
                if
                    | instruction == "ForceContinue" -> case runEval st (eval v) of
                      Right val -> set (s, val)
                      Left err -> throwError err
                    | instruction == "Break" -> step (Assign s v instruction) expre
                    | otherwise -> do
                      st <- get
                      let variableValue = Map.lookup instruction st
                      if
                          | isJust variableValue -> do
                            printout $ fromJust variableValue
                            put st
                            step (Assign s v instruction) expre
                          | otherwise -> throwError ("Unknown Variable : " ++ instruction)
            )
      Left err -> throwError err
step (Seq (CB expression instruction) s1) expre =
  do
    st <- get
    case runEval st (eval expre) of
      Right (B val) -> do
        if val && instruction == "Continue"
          then step s1 expression
          else
            ( do
                instruction <- getInput
                if
                    | instruction == "ForceContinue" -> step s1 expression
                    | instruction == "Break" -> step (Seq (CB expression instruction) s1) expre
                    | otherwise -> do
                      let variableValue = Map.lookup instruction st
                      if
                          | isJust variableValue -> do
                            printout $ fromJust variableValue
                            step (Seq (CB expression instruction) s1) expre
                          | otherwise -> throwError ("Unknown Variable : " ++ instruction)
            )
      Left err -> throwError err
step (Seq s0 s1) expre = step s0 expre >> step s1 expre
step (Print e instruction) expre =
  do
    st <- get
    case runEval st (eval expre) of
      Right (B val) -> do
        if val && instruction == "Continue"
          then
            ( case runEval st (eval e) of
                Right val -> printout val
                Left err -> throwError err
            )
          else
            ( do
                instruction <- getInput
                if
                    | instruction == "ForceContinue" -> case runEval st (eval e) of
                      Right val -> printout val
                      Left err -> throwError err
                    | instruction == "Break" -> step (Print e instruction) expre
                    | otherwise -> do
                      let variableValue = Map.lookup instruction st
                      if
                          | isJust variableValue -> do
                            printout $ fromJust variableValue
                            step (Print e instruction) expre
                          | otherwise -> throwError ("Unknown Variable : " ++ instruction)
            )
      Left err -> throwError err
step (If cond s0 s1 instruction) expre =
  do
    st <- get
    case runEval st (eval expre) of
      Right (B val) -> do
        if val && instruction == "Continue"
          then
            ( case runEval st (eval cond) of
                Right (B val) -> do
                  if val then do step s0 expre else do step s1 expre
                Left err -> throwError err
            )
          else
            ( do
                instruction <- getInput
                if
                    | instruction == "ForceContinue" -> case runEval st (eval cond) of
                      Right (B val) -> do
                        if val then do step s0 expre else do step s1 expre
                      Left err -> throwError err
                    | instruction == "Break" -> step (If cond s0 s1 instruction) expre
                    | otherwise -> do
                      let variableValue = Map.lookup instruction st
                      if
                          | isJust variableValue -> do
                            printout $ fromJust variableValue
                            step (If cond s0 s1 instruction) expre
                          | otherwise -> throwError ("Unknown Variable : " ++ instruction)
            )
step (While cond s instruction) expre =
  do
    st <- get
    case runEval st (eval expre) of
      Right (B val) -> do
        if val && instruction == "Continue"
          then
            ( case runEval st (eval cond) of
                Right (B val) -> do
                  if val then do step s expre >> step (While cond s "Continue") expre else return ()
                Left err -> throwError err
            )
          else
            ( do
                instruction <- getInput
                if
                    | instruction == "ForceContinue" -> case runEval st (eval cond) of
                      Right (B val) -> do
                        if val then do step s expre >> step (While cond s "Continue") expre else return ()
                      Left err -> throwError err
                    | instruction == "Break" -> step (While cond s instruction) expre
                    | otherwise -> do
                      let variableValue = Map.lookup instruction st
                      if
                          | isJust variableValue -> do
                            printout $ fromJust variableValue
                            step (While cond s instruction) expre
                          | otherwise -> throwError ("Unknown Variable : " ++ instruction)
            )
step (Try s0 s1 instruction) expre =
  do
    st <- get
    case runEval st (eval expre) of
      Right (B val) -> do
        if val && instruction == "Continue"
          then catchError (step s0 expre) (\e -> step s1 expre)
          else
            ( do
                instruction <- getInput
                if
                    | instruction == "ForceContinue" -> step (Try s0 s1 instruction) expre
                    | instruction == "Break" -> step (Try s0 s1 instruction) expre
                    | otherwise -> do
                      let variableValue = Map.lookup instruction st
                      if
                          | isJust variableValue -> do
                            printout $ fromJust variableValue
                            step (Try s0 s1 instruction) expre
                          | otherwise -> throwError ("Unknown Variable : " ++ instruction)
            )
step Pass expre = return ()

printout :: Val -> Run ()
printout = liftIO . System.print

getInput = liftIO System.getLine

{-------------------------------------------------------------------}
{- Pour some sugar over this -}
{-------------------------------------------------------------------}

{- This next section deals exclusively with defining convenience functions -}
{- which we will use when writing the actual programs in the DSL. -}

-- A couple of convenience functions so that we don't have to litter the program
-- with ``obvious'' constructors

int :: Int -> Expr
int = Const . I
bool :: Bool -> Expr
bool = Const . B
var :: String -> Expr
var = Var

-- The idea is that we can put a simple value on the RHS and have Haskell select the correct
-- instance by inspecting the type.

-- The assign function accepts an Instruction String as its first argument.
-- It uses the Instruction String as the thrid parameter to the Statement data constrcutor
class SmartAssignment a where
  assign :: String -> String -> a -> Statement

instance SmartAssignment Int where
  assign break v i = Assign v (Const (I i)) break

instance SmartAssignment Bool where
  assign break v b = Assign v (Const (B b)) break

instance SmartAssignment Expr where
  assign break v e = Assign v e break

-- going further with this (and only providing the classes and instances we actually usein the example program, but there could be others)

class PrettyExpr a b where
  (.*) :: a -> b -> Expr
  (.-) :: a -> b -> Expr

instance PrettyExpr String String where
  x .* y = Var x `Mul` Var y
  x .- y = Var x `Sub` Var y

instance PrettyExpr String Int where
  x .* y = Var x `Mul` Const (I y)
  x .- y = Var x `Sub` Const (I y)

{--
Making use of this we can write a program in a slightly nicer style:

I feel we're hitting the point of diminishing returns here, but I
fancy one last example of using a monad. Something to remove the need
to explicitely write "Seq" inbetween each pair of statements. Recall
that I said that >>= could be though of as a progammable semicolon?
--}

type Program = Writer Statement ()

{--
The writer monad has an operation, "tell" which appends a piece of
output to an accumulated value. For this to work the type we are
accumulating (Statement, in this case) must be have both an appending
(plus-like) operation and a base (zero-like) operation. In algebra
something with that structure is called a Monoid:
--}

-- Semigroup: how do we join two values together?
instance Semigroup Statement where
  a <> b = a `Seq` b

-- monoid: a semigroup with an identity
instance Monoid Statement where
  mempty = Pass

--  mappend a b = (<>)

{--

The idea is that the bind of the Writer monad will be used to apply
Seq (our semicolon-like operation) between each "statement". The zero
statement is needed for theoretical completeness, but it will only
appear in a result if we were to write something like this:

junk :: Program
junk = return ()

For this reason we never expect to see it in a real "compiled"
statement, so there's no case for it in the exec function.

Converting a Program to a Statement just means running the writer monad:
--}

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

{--
Executing a "program" means compiling it and then running the
resulting Statement with an empty variable map.
--}

run :: Program -> IO ()
run program = do
  result <- runExceptT $ (runStateT $ step (snd $ runIdentity (runWriterT program)) (Eq (Const $ I 1) (Const $ I 1))) Map.empty
  case result of
    Right ((), env) -> System.print env
    Left exn -> System.print ("Uncaught exception: " ++ exn)

{--
And finally some convenience functions for our syntactic sugar:
--}

-- The pretty assignment function is a binary operator. Therefore, it cannot accept more than 2 parameters.
-- I change the type of its second argument to a tuple to accept the instruction statement.
infixl 1 .=
(.=) :: String -> (Expr, String) -> Program
var .= (val, break) = tell $ assign break var val

{-- if is a keyword in Haskell so I can't hide it. I renamed it so: --}

-- iff accepts the instruction as its first argument
iif :: String -> Expr -> Program -> Program -> Program
iif break cond tthen eelse = tell $ If cond (compile tthen) (compile eelse) break

-- while accepts the instruction as its first argument
while :: String -> Expr -> Program -> Program
while break cond body = tell $ While cond (compile body) break

conditionalB :: String -> Expr -> Program
conditionalB break expr = tell $ CB expr break

{-- This is why I wanted to hide the system function "print": --}

-- print accepts the instruction as its first argument
print :: String -> Expr -> Program
print break e = tell $ Print e break

-- try accepts the instruction as its first argument
try :: String -> Program -> Program -> Program
try break block recover = tell $ Try (compile block) (compile recover) break

{--
Phew.

After all that our embedded imperative language is ready to go. Here's the factorial function in all it's glory:
--}

prog10 :: Program
prog10 = do
  "scratch" .= (var "arg", "B")
  "scratch1" .= (var "scratch2", "Continue")
  conditionalB "Continue" (int 1 `Gt` int 1)
  "unusedVar1" .= (bool True, "Break")
  "unusedVar2" .= (bool True, "Continue")
  "arg" .= (int 10, "Continue")
  "scratch" .= (var "arg", "Continue")
  "total" .= (int 1, "Continue")
  while
    "Continue"
    (var "scratch" `Gt` int 1)
    ( do
        "total" .= ("total" .* "scratch", "Continue")
        "scratch" .= ("scratch" .- (1 :: Int), "Continue")
        print "Continue" (var "scratch")
    )
  print "Continue" (var "total")

{--
to run it just evaluate

run prog10
--}

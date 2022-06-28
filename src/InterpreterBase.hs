module InterpreterBase (prog10) where

--I want these language extensions for my syntactic sugaring tricks at the end

--I want my own definition of lookup and I want to write my own function named "print".

import Prelude hiding (lookup, print)

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (
  MonadWriter (tell),
  Writer,
  WriterT (runWriterT),
 )
import qualified Data.Map as Map
import Data.Maybe
import qualified System.IO as System

import Syntax (Expr (..), Statement (..), Val (..))

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

-- type Run a = StateT Env (ExceptT String IO) a
-- runRun p = runExceptT (runStateT p Map.empty)

-- set :: (Name, Val) -> Run ()
-- set (s, i) = state (\table -> ((), Map.insert s i table))

-- -- Before executing a statement, the step function first checks the instruction argument attached to the statement
-- -- If the instruction argument is "Continue", the step function perfroms the same operation as its counterpart in exec function
-- -- If not, the step function could accepts one of three instruction, Continue, Break and a "Variable name"
-- -- With conditional Break enhenced step action
-- -- step action takes an extra Expr argument as input.
-- -- The original Expr argument should be a dummy expression which equals to True
-- -- If one of the statements of the program is a conditional break statement, the original dummy expression will be replaced by the expression listed in the conditional break statement.
-- step :: Statement -> Expr -> Run ()
-- step (Assign s v instruction) expre =
--   do
--     st <- get
--     case runEval st (eval expre) of
--       Right (B val) -> do
--         if val && instruction == "Continue"
--           then
--             ( case runEval st (eval v) of
--                 Right val -> set (s, val)
--                 Left err -> throwError err
--             )
--           else
--             ( do
--                 instruction <- getInput
--                 if
--                     | instruction == "ForceContinue" -> case runEval st (eval v) of
--                       Right val -> set (s, val)
--                       Left err -> throwError err
--                     | instruction == "Break" -> step (Assign s v instruction) expre
--                     | otherwise -> do
--                       st <- get
--                       let variableValue = Map.lookup instruction st
--                       if
--                           | isJust variableValue -> do
--                             printout $ fromJust variableValue
--                             put st
--                             step (Assign s v instruction) expre
--                           | otherwise -> throwError ("Unknown Variable : " ++ instruction)
--             )
--       Left err -> throwError err
-- step (Seq (CB expression instruction) s1) expre =
--   do
--     st <- get
--     case runEval st (eval expre) of
--       Right (B val) -> do
--         if val && instruction == "Continue"
--           then step s1 expression
--           else
--             ( do
--                 instruction <- getInput
--                 if
--                     | instruction == "ForceContinue" -> step s1 expression
--                     | instruction == "Break" -> step (Seq (CB expression instruction) s1) expre
--                     | otherwise -> do
--                       let variableValue = Map.lookup instruction st
--                       if
--                           | isJust variableValue -> do
--                             printout $ fromJust variableValue
--                             step (Seq (CB expression instruction) s1) expre
--                           | otherwise -> throwError ("Unknown Variable : " ++ instruction)
--             )
--       Left err -> throwError err
-- step (Seq s0 s1) expre = step s0 expre >> step s1 expre
-- step (Print e instruction) expre =
--   do
--     st <- get
--     case runEval st (eval expre) of
--       Right (B val) -> do
--         if val && instruction == "Continue"
--           then
--             ( case runEval st (eval e) of
--                 Right val -> printout val
--                 Left err -> throwError err
--             )
--           else
--             ( do
--                 instruction <- getInput
--                 if
--                     | instruction == "ForceContinue" -> case runEval st (eval e) of
--                       Right val -> printout val
--                       Left err -> throwError err
--                     | instruction == "Break" -> step (Print e instruction) expre
--                     | otherwise -> do
--                       let variableValue = Map.lookup instruction st
--                       if
--                           | isJust variableValue -> do
--                             printout $ fromJust variableValue
--                             step (Print e instruction) expre
--                           | otherwise -> throwError ("Unknown Variable : " ++ instruction)
--             )
--       Left err -> throwError err
-- step (If cond s0 s1 instruction) expre =
--   do
--     st <- get
--     case runEval st (eval expre) of
--       Right (B val) -> do
--         if val && instruction == "Continue"
--           then
--             ( case runEval st (eval cond) of
--                 Right (B val) -> do
--                   if val then do step s0 expre else do step s1 expre
--                 Left err -> throwError err
--             )
--           else
--             ( do
--                 instruction <- getInput
--                 if
--                     | instruction == "ForceContinue" -> case runEval st (eval cond) of
--                       Right (B val) -> do
--                         if val then do step s0 expre else do step s1 expre
--                       Left err -> throwError err
--                     | instruction == "Break" -> step (If cond s0 s1 instruction) expre
--                     | otherwise -> do
--                       let variableValue = Map.lookup instruction st
--                       if
--                           | isJust variableValue -> do
--                             printout $ fromJust variableValue
--                             step (If cond s0 s1 instruction) expre
--                           | otherwise -> throwError ("Unknown Variable : " ++ instruction)
--             )
-- step (While cond s instruction) expre =
--   do
--     st <- get
--     case runEval st (eval expre) of
--       Right (B val) -> do
--         if val && instruction == "Continue"
--           then
--             ( case runEval st (eval cond) of
--                 Right (B val) -> do
--                   if val then do step s expre >> step (While cond s "Continue") expre else return ()
--                 Left err -> throwError err
--             )
--           else
--             ( do
--                 instruction <- getInput
--                 if
--                     | instruction == "ForceContinue" -> case runEval st (eval cond) of
--                       Right (B val) -> do
--                         if val then do step s expre >> step (While cond s "Continue") expre else return ()
--                       Left err -> throwError err
--                     | instruction == "Break" -> step (While cond s instruction) expre
--                     | otherwise -> do
--                       let variableValue = Map.lookup instruction st
--                       if
--                           | isJust variableValue -> do
--                             printout $ fromJust variableValue
--                             step (While cond s instruction) expre
--                           | otherwise -> throwError ("Unknown Variable : " ++ instruction)
--             )
-- step (Try s0 s1 instruction) expre =
--   do
--     st <- get
--     case runEval st (eval expre) of
--       Right (B val) -> do
--         if val && instruction == "Continue"
--           then catchError (step s0 expre) (\e -> step s1 expre)
--           else
--             ( do
--                 instruction <- getInput
--                 if
--                     | instruction == "ForceContinue" -> step (Try s0 s1 instruction) expre
--                     | instruction == "Break" -> step (Try s0 s1 instruction) expre
--                     | otherwise -> do
--                       let variableValue = Map.lookup instruction st
--                       if
--                           | isJust variableValue -> do
--                             printout $ fromJust variableValue
--                             step (Try s0 s1 instruction) expre
--                           | otherwise -> throwError ("Unknown Variable : " ++ instruction)
--             )
-- step Pass expre = return ()

-- printout :: Val -> Run ()
-- printout = liftIO . System.print

-- getInput = liftIO System.getLine

-- {-------------------------------------------------------------------}
-- {- Pour some sugar over this -}
-- {-------------------------------------------------------------------}

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
  assign :: String -> a -> Statement

instance SmartAssignment Int where
  assign v i = Assign v (Const (I i))

instance SmartAssignment Bool where
  assign v b = Assign v (Const (B b))

instance SmartAssignment Expr where
  assign = Assign

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
something with that structure is called a Monoid
--}

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

-- run :: Program -> IO ()
-- run program = do
--   result <- runExceptT $ (runStateT $ step (snd $ runIdentity (runWriterT program)) (Eq (Const $ I 1) (Const $ I 1))) Map.empty
--   case result of
--     Right ((), env) -> System.print env
--     Left exn -> System.print ("Uncaught exception: " ++ exn)

{--
And finally some convenience functions for our syntactic sugar:
--}

-- The pretty assignment function is a binary operator. Therefore, it cannot accept more than 2 parameters.
-- I change the type of its second argument to a tuple to accept the instruction statement.
infixl 1 .=
(.=) :: Name -> Expr -> Program
variable .= val = tell $ assign variable val

{-- if is a keyword in Haskell so I can't hide it. I renamed it so: --}

-- iff accepts the instruction as its first argument
iif :: Expr -> Program -> Program -> Program
iif cond tthen eelse = tell $ If cond (compile tthen) (compile eelse)

-- while accepts the instruction as its first argument
while :: Expr -> Program -> Program
while cond body = tell $ While cond (compile body)

{-- This is why I wanted to hide the system function "print": --}

-- print accepts the instruction as its first argument
print :: Expr -> Program
print e = tell $ Print e

-- try accepts the instruction as its first argument
-- try :: String -> Program -> Program -> Program
-- try break block recover = tell $ Try (compile block) (compile recover) break

{--
Phew.

After all that our embedded imperative language is ready to go. Here's the factorial function in all it's glory:
--}

prog10 :: Program
prog10 = do
  "scratch" .= var "arg"
  "scratch1" .= var "scratch2"
  "unusedVar1" .= bool True
  "unusedVar2" .= bool True
  "arg" .= int 10
  "scratch" .= var "arg"
  "total" .= int 1
  while
    (var "scratch" `Gt` int 1)
    ( do
        "total" .= ("total" :: String) .* ("scratch" :: String)
        "scratch" .= ("scratch" :: String) .- (1 :: Int)
        print (var "scratch")
    )
  print $ var "total"

{--
to run it just evaluate

run prog10
--}

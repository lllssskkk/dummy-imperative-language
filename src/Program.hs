module Program (Val (..), Expr (..), Statement (..), Program, int, bool, var, (.*), (.-), (.=), assign, compile, iif, while, breakpoint, printt, function, call) where

import Constant
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Writer (Writer, WriterT (runWriterT), tell)
import Prelude hiding (print)

data Val where
    I :: Int -> Val
    B :: Bool -> Val
    Double :: Double -> Val
    Closure :: [VarName] -> Expr -> Val
    deriving stock (Eq, Show)

data Expr where
    Const :: Val -> Expr
    And :: Expr -> Expr -> Expr
    Or :: Expr -> Expr -> Expr
    Not :: Expr -> Expr
    Eq :: Expr -> Expr -> Expr
    Gt :: Expr -> Expr -> Expr
    Lt :: Expr -> Expr -> Expr
    Add :: Expr -> Expr -> Expr
    Sub :: Expr -> Expr -> Expr
    Mul :: Expr -> Expr -> Expr
    Div :: Expr -> Expr -> Expr
    Lambda :: [VarName] -> Expr -> Expr
    Var :: VarName -> Expr
    deriving stock (Eq, Show)

data Statement where
    Assign :: VarName -> Expr -> Statement
    Function :: FnName -> [VarName] -> Expr -> Statement
    Call :: FnName -> [Val] -> VarName -> Statement
    If :: Expr -> Statement -> Statement -> Statement
    While :: Expr -> Statement -> Statement
    Print :: String -> Statement
    Seq :: Statement -> Statement -> Statement
    Break :: Statement
    Pass :: Statement
    deriving stock (Eq, Show)

instance Semigroup Statement where
    a <> b = a `Seq` b

-- monoid: a semigroup with an identity
instance Monoid Statement where
    mempty = Pass

type Program = Writer Statement ()

int :: Int -> Expr
int = Const . I
bool :: Bool -> Expr
bool = Const . B
var :: String -> Expr
var = Var

class SmartAssignment a where
    assign :: String -> a -> Statement

instance SmartAssignment Int where
    assign v i = Assign v (Const (I i))

instance SmartAssignment Bool where
    assign v b = Assign v (Const (B b))

instance SmartAssignment Expr where
    assign = Assign

class PrettyExpr a b where
    (.*) :: a -> b -> Expr
    (.-) :: a -> b -> Expr

instance PrettyExpr String String where
    x .* y = Var x `Mul` Var y
    x .- y = Var x `Sub` Var y

instance PrettyExpr String Int where
    x .* y = Var x `Mul` Const (I y)
    x .- y = Var x `Sub` Const (I y)

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

infixl 1 .=
(.=) :: VarName -> Expr -> Program
variable .= val = tell $ assign variable val

-- iff accepts the instruction as its first argument
iif :: Expr -> Program -> Program -> Program
iif cond tthen eelse = tell $ If cond (compile tthen) (compile eelse)

-- while accepts the instruction as its first argument
while :: Expr -> Program -> Program
while cond body = tell $ While cond (compile body)

-- breakpoint
breakpoint :: Program
breakpoint = tell Break

-- function
function :: FnName -> [VarName] -> Expr -> Program
function fnName bindings body = tell $ Function fnName bindings body

-- function calls
call :: FnName -> [Val] -> VarName -> Program
call fnName inputs varName = tell $ Call fnName inputs varName

{-- This is why I wanted to hide the system function "print": --}

-- print accepts the instruction as its first argument
printt :: VarName -> Program
printt e = tell $ Print e

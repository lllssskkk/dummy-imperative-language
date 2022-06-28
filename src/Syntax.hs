module Syntax (Val (..), Expr (..), Statement (..)) where

data Val where
    I :: Int -> Val
    B :: Bool -> Val
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
    Var :: String -> Expr
    deriving stock (Eq, Show)

data Statement where
    Assign :: String -> Expr -> Statement
    If :: Expr -> Statement -> Statement -> Statement
    While :: Expr -> Statement -> Statement
    Print :: Expr -> Statement
    Seq :: Statement -> Statement -> Statement
    Break :: Statement
    Pass :: Statement
    deriving stock (Eq, Show)

instance Semigroup Statement where
    a <> b = a `Seq` b

-- monoid: a semigroup with an identity
instance Monoid Statement where
    mempty = Pass

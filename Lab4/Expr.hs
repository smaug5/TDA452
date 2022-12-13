module Expr where

data Op = Add | Sub | Mul
            deriving (Eq, Show)

data Func = Sin | Cos
            deriving (Eq, Show)

data Expr =  Num Double
           | Operation Op Expr Expr 
           | Function Func Expr
           | Variable Char
           deriving (Eq, Show)

-- sin(2+9)
-- cos(2*3)+4*x
testEXPR = Operation Add (Function Cos (Operation Mul (Num 2) (Num 3))) (Operation Mul (Num 4) (Variable 'x'))

-------A---------------------------------------------------------------------------

x :: Expr
x = Variable 'x'

num :: Double -> Expr
num value = Num value

add :: Expr -> Expr -> Expr
add value value' = Operation Add value value'

mul :: Expr -> Expr -> Expr
mul value value' = Operation Mul value value'

sin :: Expr -> Expr
sin value = Function Sin value


cos :: Expr -> Expr
cos value = Function Cos value


size :: Expr -> Int
size (Num _)                   = 0
size (Variable _)              = 0
size (Function _ expr)         = 1 + size expr
size (Operation _ expr expr' ) = 1 + size expr + size expr'


--------B-----------------------------------------------------------------------

showExpr :: Expr -> String
showExpr (Num num)                 = show num
showExpr (Variable var)            = "x"
showExpr (Function func expr)      = showFunction (Function func expr)
showExpr (Operation op expr expr') = showExpr expr ++ showOperator op  ++ showExpr expr'


showOperator :: Op -> String
showOperator Add = " + "
showOperator Mul = "*"

showFunction :: Expr -> String
showFunction (Function Sin expr) = "sin(" ++ showExpr expr ++ ")"
showFunction (Function Cos expr) = "cos(" ++ showExpr expr ++ ")"


----------C---------------------------------------------------------------------------

eval :: Expr -> Double -> Double
eval expr value = 
module Expr where

data Op = Add | Sub | Mul

data Func = Sin | Cos

data Expr = Num Double | Operation Op Expr Expr | Function Func Expr | Variable Char


x :: Expr
x = Variable 'x'

num :: Double -> Expr
num value = Num value

add :: Expr -> Expr -> Expr
add value value' = Operation Add value value'

mul :: Expr -> Expr -> Expr
mul value value' = Operation Mul value value'

sin :: Expr -> Expr


cos :: Expr -> Expr


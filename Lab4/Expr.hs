module Expr where
import Parsing
import Data.Maybe
import Test.QuickCheck


data Op = Add | Mul
            deriving (Eq, Show)

data Func = Sin | Cos
            deriving (Eq, Show)

data Expr =  Num Double
           | Operation Op Expr Expr 
           | Function Func Expr
           | VarX
           deriving (Eq, Show)

-- sin(2+9)
-- cos(2*3)+4*x
expr1 = Operation Add (Function Cos (Operation Mul (Num 2) (Num 3))) (Operation Mul (Num 4) (VarX))

expr2 = Function Cos (Operation Mul (Num 2) (Num 3))

strAdd        = "1.0+2.0"
strFunc       = "sin1"
strMul        = "2*3"
strPar        = "(1+1)"
strFuncMul    = "sin(2*3)"
strAddFuncMul = "1+sin(2*3)"

-------A---------------------------------------------------------------------------

x :: Expr
x = VarX

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
size (Num _)                   = 1 -- Due to feedback from review
size (VarX)                    = 1 -- Due to feedback from review
size (Function _ expr)         = 1 + size expr
size (Operation _ expr expr' ) = 1 + size expr + size expr'
-- In the description for the lab it says we should count "Functions" and "Operators", which would exclude variables and numbers to be base case 0.


--------B-----------------------------------------------------------------------

showExpr :: Expr -> String
showExpr (Num num)                 = show num
showExpr (VarX)                    = "x"
showExpr (Function func expr)      = showFunction (Function func expr)
showExpr (Operation op expr expr') = convert (Operation op expr expr') expr ++ showOperator op  ++
                                      convert (Operation op expr expr') expr'

higherPrec :: Expr -> Expr -> Bool
higherPrec (Operation Mul e11 e12) (Operation Add e21 e22) = True
higherPrec (Function Sin e1) (Operation Mul e21 e22) = True
higherPrec (Function Sin e1) (Operation Add e21 e22) = True
higherPrec (Function Cos e1) (Operation Mul e21 e22) = True
higherPrec (Function Cos e1) (Operation Add e21 e22) = True
higherPrec e1 e2 = False

convert :: Expr -> Expr -> String
convert e1 e2
  | e1 `higherPrec` e2 = "(" ++ showExpr e2 ++ ")"
  | otherwise = showExpr e2

showOperator :: Op -> String
showOperator Add = "+"
showOperator Mul = "*"

showFunction :: Expr -> String
showFunction (Function Sin expr) = "sin(" ++ showExpr expr ++ ")"
showFunction (Function Cos expr) = "cos(" ++ showExpr expr ++ ")"


----------C---------------------------------------------------------------------------

eval :: Expr -> Double -> Double
eval (Num num) _                  = num
eval VarX x                       = x
eval (Function Sin expr) x        = Prelude.sin(eval expr x)
eval (Function Cos expr) x        = Prelude.cos(eval expr x)
eval (Operation Mul expr expr') x = eval expr x * eval expr' x
eval (Operation Add expr expr') x = eval expr x + eval expr' x


--------D-----------------------------------------------------------------------------
parseVariable :: Parser Expr
parseVariable = do
    char 'x'
    return (VarX)

parseNumber :: Parser Expr
parseNumber = do
    ds <- oneOrMore digit
    return (Num (read ds))

parseNegative :: Parser Expr
parseNegative = do
    char '-'
    ds <- oneOrMore digit
    return (Num (read ("-" ++ ds)))

parseNumberWithDec :: Parser Expr
parseNumberWithDec = do
    ds <- oneOrMore digit
    char '.'
    decimals <- oneOrMore digit
    return (Num (read (ds ++ "." ++ decimals)))

parseNegativeWithDec :: Parser Expr
parseNegativeWithDec = do
    char '-'
    ds <- oneOrMore digit
    char '.'
    decimals <- oneOrMore digit
    return (Num (read ("-" ++ ds ++ "." ++ decimals)))

parsePar :: Parser Expr
parsePar = do
    char '('
    e <- parseOp
    char ')'
    return e


parseSin :: Parser Expr
parseSin = do
    char 's'
    char 'i'
    char 'n'
    e <- parseTerm
    return $ (Function Sin) e

parseCos :: Parser Expr
parseCos = do
    char 'c'
    char 'o'
    char 's'
    e <- parseTerm

    return $ (Function Cos) e

parseFunction :: Parser Expr
parseFunction = parseSin <|> parseCos

parseTerm :: Parser Expr
parseTerm = parsePar <|> parseFunction <|> parseNumberWithDec <|> parseNumber <|> parseVariable


parseAdd :: Parser Expr
parseAdd = do
    term <- parseMul <|> parseTerm
    terms <- zeroOrMore (do
                char '+'
                parseMul <|> parseTerm)
    return $ foldl (Operation Add) term terms


parseMul :: Parser Expr
parseMul = do
    term <- parseTerm
    terms <- zeroOrMore (do 
        char '*'
        parseTerm)
    return $ foldl (Operation Mul) term terms

parseOp :: Parser Expr
parseOp = parseAdd <|> parseMul

readExpr :: String -> Maybe Expr
readExpr strx = do
    (e, str) <- parse parseOp str
    return e
    where 
        str = filter (/=' ') strx




assoc :: Expr -> Expr
assoc (Operation Add (Operation Add e1 e2) e3)  = assoc (Operation Add e1 (Operation Add e2 e3))
assoc (Operation Add e1          e2)            = Operation Add (assoc e1) (assoc e2)
assoc (Operation Mul (Operation Mul e1 e2) e3)  = assoc $ Operation Mul e1 $ Operation Mul e2 e3
assoc (Operation Mul e1          e2)            = Operation Mul (assoc e1) (assoc e2)
assoc  VarX                                     = VarX
assoc (Function Sin e)                          = Function Sin $ assoc e
assoc (Function Cos e)                          = Function Cos $ assoc e
assoc (Num n)                                   = Num n

---------E----------------------------------------------------------------------------

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr = (fromJust $ readExpr $ showExpr expr) == expr

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1, arbNum), (s, arbBin s), (s, arbFunc s)]
    where 
        arbNum = do
            Positive n <- arbitrary
            return $ Num n
        arbBin s = do
            bin <- elements [Operation Add, Operation Mul]
            e1 <- arbExpr $ s `div` 2
            e2 <- arbExpr $ s `div` 2
            return $ bin e1 e2    
        arbFunc s = do
            f <- elements [Function Sin, Function Cos]
            e <- arbExpr $ s `div` 2
            return $ f e
          



--Num <$> choose (1.0, 99.0)

instance Arbitrary Expr where
    arbitrary = sized arbExpr

------F-------------------------------------------------------------------------------

simplify :: Expr -> Expr
-- Simply a number or a variable
simplify VarX    = VarX
simplify (Num n) = Num n

-- Multiplication with zero
simplify (Operation Mul (Num 0) _ ) = Num 0
simplify (Operation Mul _ (Num 0))  = Num 0

-- Multiplication with one
simplify (Operation Mul (Num 1.0) e) = e
simplify (Operation Mul e (Num 1.0)) = e

-- Multification with variable or 2 simple numbers
simplify (Operation Mul e VarX) = Operation Mul (simplify e) VarX
simplify (Operation Mul VarX e) = Operation Mul VarX (simplify e)
simplify (Operation Mul (Num n1) (Num n2)) = Num (n1 * n2)

-- Multiplication between expressions
simplify (Operation Mul e1 e2)
    | e1' == e1 && e2' == e2 = (Operation Mul) e1 e2
    | e1' == e1              = simplify $ Operation Mul e1 e2'
    | e2' == e2              = simplify $ Operation Mul e1' e2
    | otherwise              = simplify $ Operation Mul e1' e2'
      where e1' = simplify e1
            e2' = simplify e2

-- Addition
simplify (Operation Add VarX (Num 0)) = VarX
simplify (Operation Add (Num 0) VarX) = VarX
simplify (Operation Add VarX VarX)    = Operation Mul (Num 2) VarX

-- Simplifying expressions with additions
simplify (Operation Add e VarX)            = Operation Add (simplify e) VarX
simplify (Operation Add VarX e)            = Operation Add VarX (simplify e)
simplify (Operation Add (Num n1) (Num n2)) = Num (n1 + n2)
simplify (Operation Add e1 e2)
    | e1' == e1 && e2' == e2 = Operation Add e1 e2
    | e1' == e1              = simplify $ Operation Add e1 e2'
    | e2' == e2              = simplify $ Operation Add e1' e2
    | otherwise              = simplify $ Operation Add e1' e2'
      where e1' = simplify e1
            e2' = simplify e2

simplify (Function Sin (Num n)) = Num $ Prelude.sin n
simplify (Function Cos (Num n)) = Num $ Prelude.cos n

simplify (Function Sin e)
    | e' == e = Function Sin e
    | otherwise = simplify $ Function Sin e'
      where e' = simplify e

simplify (Function Cos e)
    | e' == e = Function Cos e
    | otherwise = simplify $ Function Cos e'
      where e' = simplify e


prop_Simplify :: Expr -> Bool
prop_Simplify e = eval (simplify e) 1 == eval e 1


-----G--------------------------------------------------------------------------------

differentiate :: Expr -> Expr
differentiate e = simplify $ differentiate' $ simplify e


-- sin(f(x)) = f'(x) (cos(f(x))
-- cos(f(x)) = f'(x) (-sin(f(x)))
differentiate' :: Expr -> Expr
differentiate' (Function Sin e) = Operation Mul (differentiate e) (Function Cos e)
differentiate' (Function Cos e) = Operation Mul (differentiate e) (Operation Mul (Num (-1)) (Function Sin e))

differentiate' (Operation Add e1 e2) = Operation Add (differentiate e1) (differentiate e2)
differentiate' (Operation Mul e1 e2) = Operation Add (Operation Mul (differentiate e1) e2) (Operation Mul e1 (differentiate e2))

differentiate' VarX = Num 1
differentiate' (Num n) = Num 0
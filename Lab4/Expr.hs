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
size (Num _)                   = 0
size (VarX)                    = 0
size (Function _ expr)         = 1 + size expr
size (Operation _ expr expr' ) = 1 + size expr + size expr'


--------B-----------------------------------------------------------------------

showExpr :: Expr -> String
showExpr (Num num)                 = show num
showExpr (VarX)                    = "x"
showExpr (Function func expr)      = showFunction (Function func expr)
showExpr (Operation op expr expr') = showExpr expr ++ showOperator op  ++ showExpr
                                     expr'


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

parseNumberWithDec :: Parser Expr
parseNumberWithDec = do
    ds <- oneOrMore digit
    char '.'
    decimals <- oneOrMore digit
    return (Num (read (ds ++ "." ++ decimals)))

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
    e <- parseOp
    return $ (Function Sin) e

parseCos :: Parser Expr
parseCos = do
    char 'c'
    char 'o'
    char 's'
    e <- parseOp

    return $ (Function Cos) e

parseFunction :: Parser Expr
parseFunction = parseSin <|> parseCos

parseTerm :: Parser Expr
parseTerm = parsePar <|> parseFunction <|> parseNumberWithDec <|> parseNumber <|> parseVariable


parseAdd :: Parser Expr
parseAdd = do
    term <- parseTerm
    terms <- zeroOrMore (do
                char '+'
                parseTerm)
    return $ foldl (Operation Add) term terms


parseMul :: Parser Expr
parseMul = do
    term <- parseAdd <|> parseTerm
    terms <- zeroOrMore (do 
        char '*'
        parseTerm)
    return $ foldl (Operation Mul) term terms

parseOp :: Parser Expr
parseOp = parseMul <|> parseAdd

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
    where arbNum = elements $ map Num [1,n]
          arbBin s = do
            bin <- elements [Operation Add, Operation Mul]
            e1 <- arbExpr $ s `div` 2
            e2 <- arbExpr $ s `div` 2
            return $ bin e1 e2
          
          arbFunc s = do
            f <- elements [Function Sin, Function Cos]
            e <- arbExpr $ s `div` 2
            return $ f e
          
          n = 7



--Num <$> choose (1.0, 99.0)

instance Arbitrary Expr where
    arbitrary = sized arbExpr
import Data.Char
import Data.Fixed

-- Bohdan Perederei, KM-03

-- TASK 1. NEWTON METHOD
diff f dx x = (f(x+dx) - f(x)) / dx

newton_iter f f' x k
        | k == 0    = x
        | otherwise = newton_iter f f' closerx (k-1)
        where closerx = x - (f(x) / f'(x))

task1 = do
    let fun1 = \x -> sin x
    let fun2 = \x -> x^^3 - 328*x^^2 - 1999*x - 1670
    putStrLn "Function sin x | dx = 0.01 | x = 0.5 | k = 1000"
    print $ newton_iter fun1 (diff fun1 0.01) 0.5 1000
    putStrLn "Function x^3 - 328*x^2 - 1999*x - 1670 | dx = 0.01 | x = 100 | k = 1000"
    print $ newton_iter fun2 (diff fun2 0.01) 100 1000

-- TASK 2. FUNCTIONS
type IntSet = (Int -> Bool)

isMember :: IntSet -> Int -> Bool
isMember f x = f x

emptySet :: IntSet
emptySet x = False

allInts :: IntSet
allInts x = True

-- interval x y contains all the integers in [x;y]
interval :: Int -> Int -> IntSet
interval lBound uBound = \x -> x >= lBound && x <= uBound

gcd_euclid :: Int -> Int -> Int
gcd_euclid x y
        | y == 0    = x
        | x == 0    = y
        | otherwise = gcd_euclid y (x `mod` y)

coprime :: Int -> IntSet
coprime k = \x -> (gcd_euclid k x == 1)

-- Boolean Operators
setIntersection :: IntSet -> IntSet -> IntSet
setIntersection set1 set2 = \x -> (set1 x) && (set2 x)

setUnion :: IntSet -> IntSet -> IntSet
setUnion set1 set2 = \x -> (set1 x) || (set2 x)

setComplement :: IntSet -> IntSet -> IntSet
setComplement set1 set2 = \x -> (set1 x) && not (set2 x)

-- Set generation
addToSet :: Int -> IntSet -> IntSet
addToSet element set
        | isMember set element = set
        | otherwise            = \x -> (x == element) || (set x)

deleteFromSet :: Int -> IntSet -> IntSet
deleteFromSet element set
        | not (isMember set element) = set
        | otherwise                  = \x -> (x /= element) && (set x)  

task2 = do
    putStrLn $ "Interval tests"
    putStrLn $ "25 is emptySet: " ++ show (emptySet 25)
    putStrLn $ "25 is in allInts: " ++ show (allInts 7)
    putStrLn $ "25 is in [10; 30]: " ++ show ((interval 10 30) 25)
    putStrLn $ "25 is in [10; 20]: " ++ show ((interval 10 20) 25)
    putStrLn $ ""
    
    putStrLn $ "Coprime set tests"
    putStrLn $ "Coprime 25 23: " ++ show ((coprime 25) 23)
    putStrLn $ "Coprime 25 13: " ++ show ((coprime 25) 13)
    putStrLn $ "Coprime 25 5: " ++ show ((coprime 25) 5)
    putStrLn $ ""

    putStrLn $ "Set operations tests"
    putStrLn $ "25 in intersection [10; 15] [13; 25]: " ++ show ((setIntersection (interval 10 15) (interval 13 25)) 25)
    putStrLn $ "25 in union [10; 15] [20; 30]: " ++ show ((setUnion (interval 10 15) (interval 20 30)) 25)
    putStrLn $ "25 in complement [10; 50] [20; 30]: " ++ show ((setComplement (interval 10 50) (interval 20 30)) 25)
    putStrLn $ "25 in addToSet 25 [10; 20]: " ++ show ((addToSet 25 (interval 10 20)) 25)
    putStrLn $ "25 in deleteFromSet 25 [10; 30]: " ++ show ((deleteFromSet 25 (interval 10 30)) 25)

    
-- TASK 3. PARSING EXPRESSIONS
data Token = Number Float | Operator Char | Function String | Variable String deriving (Eq, Show)
type Stack = [Token]

-- Operators/Functions priority
priority :: Token -> Int
priority (Operator token)
        | token == '+'  = 1
        | token == '-'  = 1
        | token == '*'  = 2
        | token == '/'  = 2
        | token == '%'  = 3
        | token == '^'  = 4
        | token == '('  = 1 -- It means that parser should check for brackets by itself
        | token == ')'  = 1 -- and doesn't depend on priority of brackets
        | otherwise = 0
priority (Function token)
        | token == "sin" = 5
        | token == "cos" = 5
        | token == "tan" = 5
        | otherwise = 0
priority _ = 0

numLength :: String -> Int
numLength [] = 0
numLength (x:xs)
        | isDigit x = 1 + numLength xs
        | x == '.'  = 1 + numLength xs
        | otherwise = 0

varLength :: String -> Int
varLength [] = 0
varLength (x:xs)
        | isAlpha x                                  = 1 + varLength xs
        | x == ' '                                   = 0
        | (priority (Operator x) /= 0) && (x /= '(') = 0
        | otherwise                                  = error "Not compatible name for a varaible"

stringToTokenList :: String -> [Token]
stringToTokenList [] = []
stringToTokenList str
        | isDigit (head str)                  = Number (read (take (numLength str) str) :: Float) : stringToTokenList (drop (numLength str) str)
        | head str == ' '                     = stringToTokenList (tail str)
        | priority (Operator (head str)) /= 0 = Operator (head str) : stringToTokenList (tail str)
        | take 3 str == "sin"                 = Function "sin" : stringToTokenList (drop 3 str)
        | take 3 str == "cos"                 = Function "cos" : stringToTokenList (drop 3 str)
        | take 3 str == "tan"                 = Function "tan" : stringToTokenList (drop 3 str)
        | isAlpha (head str)                  = Variable (take (varLength str) str) : stringToTokenList (drop (varLength str) str)
        | otherwise                           = error "Undefined token"

-- Returns number of tokens which should be put from operator stack to output queue
pushDepth :: Token -> Stack -> Int
pushDepth operator [] = 0
pushDepth operator (x:xs)
        | operator == Operator ')' && x == Operator '('              = 1
        | operator == Operator ')'                                   = 1 + pushDepth operator xs
        | priority operator < priority x && operator /= Operator '(' = 1 + pushDepth operator xs 
        | otherwise                                                  = 0

parsing :: [Token] -> Stack -> Stack -> Stack
parsing [] operatorStack outputQueue = outputQueue ++ operatorStack
parsing ((Number x):xs) operatorStack outputQueue = parsing xs operatorStack (outputQueue ++ [Number x])
parsing ((Variable x):xs) operatorStack outputQueue = parsing xs operatorStack (outputQueue ++ [Variable x])
parsing (x:xs) operatorStack outputQueue
        | x == Operator '('  = parsing xs (x : operatorStack) outputQueue
        | x == Operator ')'  = parsing xs (drop pushStackDepth operatorStack) (outputQueue ++ take (pushStackDepth-1) operatorStack)
        | priority x /= 0    = parsing xs (x : drop pushStackDepth operatorStack) (outputQueue ++ take pushStackDepth operatorStack) 
        | otherwise          = error "Unidentified token"
        where pushStackDepth = pushDepth x operatorStack

parse :: String -> Stack
parse []  = []
parse str = parsing (stringToTokenList str) [] []

performOperationOnTokens :: Token -> [Float] -> [Float]
performOperationOnTokens (Operator operator) (x:xs)
        | operator == '+' = head xs + x : tail xs
        | operator == '-' = head xs - x : tail xs
        | operator == '*' = head xs * x : tail xs
        | operator == '/' = head xs / x : tail xs
        | operator == '%' = head xs `mod'` x : tail xs
        | operator == '^' = head xs ** x : tail xs
        | otherwise       = error (show operator ++ "isn't specified operation")
performOperationOnTokens (Function func) (x:xs)
        | func == "sin" = sin(x) : xs
        | func == "cos" = cos(x) : xs
        | func == "tan" = tan(x) : xs
        | otherwise     = error (show func ++ "isn't specified function")
performOperationOnTokens _ _ = error "Stack doesn't have enough elements to perform operation"

evaling :: Stack -> [Float] -> Float
evaling [] numbersStack                = head numbersStack
evaling ((Number x):xs) numbersStack   = evaling xs (x : numbersStack)
evaling ((Variable x):xs) numbersStack = error (show (Variable x) ++ " hasn't been specified")
evaling (x:xs) numbersStack            = evaling xs (performOperationOnTokens x numbersStack)

substitute :: Token -> [[Token]] -> Token
substitute (Variable var) [] = error (show (Variable var) ++ " isn't specified")
substitute (Variable var) (x:xs)
        | Variable var == head x = last x
        | otherwise              = substitute (Variable var) xs
substitute element _ = element

representVariables :: Stack -> [[Token]] -> Stack
representVariables [] _                   = []
representVariables stack []               = stack 
representVariables ((Variable x):xs) vars = substitute (Variable x) vars : representVariables xs vars
representVariables (x:xs) vars            = x : representVariables xs vars

eval :: Stack -> [[Token]] -> Float
eval expr vars = evaling (representVariables expr vars) []

test_expr expr vars = do
    putStrLn $ "Parse " ++ expr
    putStrLn $ show (parse expr)
    putStrLn $ "Eval " ++ expr ++ " | Variables: " ++ show vars
    putStrLn $ show (eval (parse expr) vars)
    putStrLn $ ""

task3 = do
    let expr1 = "10 * 2 ^ (3 - 1) * 3.5"
    let expr2 = "(10 / (2 % 2)) + 1"
    let expr3 = "((2 + 2)) + (((3 ^ 2 % 2)))"
    let expr4 = "(sin(x) + cos(x) + y) * sin(x / 2) + tan(x)"
    let expr5 = "(x * y) + sin(ab) * 12"
    test_expr expr1 []
    test_expr expr2 []
    test_expr expr3 []
    test_expr expr4 [[Variable "x", Number pi], [Variable "y", Number 10]]
    test_expr expr5 [[Variable "x", Number (-5.5)], [Variable "y", Number 12], [Variable "ab", Number (pi/2)]]

-- TASK 4. DUAL NUMBERS 
data Dual a = Dual a a deriving (Show)

real :: Dual a -> a
real (Dual r _) = r

dual :: Dual a -> a
dual (Dual _ d) = d

instance Eq a => Eq (Dual a) where
    (==) (Dual r1 d1) (Dual r2 d2) = (r1 == r2) && (d1 == d2)

instance Num a => Num (Dual a) where
    (+) (Dual r1 d1) (Dual r2 d2) = Dual (r1 + r2) (d1 + d2)
    (-) (Dual r1 d1) (Dual r2 d2) = Dual (r1 - r2) (d1 - d2)
    (*) (Dual r1 d1) (Dual r2 d2) = Dual (r1 * r2) (r1 * d2 + r2 * d1)

    negate (Dual r d)             = Dual (negate r) (negate d)
    abs (Dual r d)                = Dual (abs r) (d * (signum r))
    signum (Dual r d)             = Dual (signum r) 0
    fromInteger a                 = Dual (fromInteger a) 0

instance Fractional a => Fractional (Dual a) where
    (/) (Dual r1 d1) (Dual r2 d2) = Dual (r1 / r2) ((d1 * r2 - r1 * d2) / r2 ^ 2)
    recip (Dual r d)              = Dual (recip r) (-1 * d * (recip (r ^ 2)))
    fromRational n                = Dual (fromRational n) 0

instance Floating a => Floating (Dual a) where
    pi = Dual pi 0

    (**) (Dual r1 d1) (Dual r2 d2)= Dual (r1 ** r2) (r1 ** r2 * (d2 * (log r1) + (r2 * d1 / r1)))
    sqrt (Dual r d)  = Dual (sqrt r) (d / (2 * sqrt r))

    acos (Dual r d)  = Dual (acos r) (- d / (sqrt(1 - r * r)))
    asin (Dual r d)  = Dual (asin r) (d / (sqrt(1 - r * r)))
    atan (Dual r d)  = Dual (atan r) (d / (1 + r * r))

    cos (Dual r d)   = Dual (cos r) (-d * sin r)
    sin (Dual r d)   = Dual (sin r) (d * cos r)
    tan (Dual r d)   = Dual (tan r) (1 / ((cos r) ** 2))

    acosh (Dual r d) = Dual (acosh r) (d / sqrt(r ** 2 - 1))
    asinh (Dual r d) = Dual (asinh r) (d / sqrt(1 + r ** 2))
    atanh (Dual r d) = Dual (atanh r) (d / (1 - r ** 2))

    cosh (Dual r d)  = Dual (cosh r) (d * sinh r)
    sinh (Dual r d)  = Dual (sinh r) (d * cosh r) 
    tanh (Dual r d)  = Dual (tanh r) (d * (1 - (tanh r) ** 2))
    
    exp (Dual r d)   = Dual (exp r) (d * (exp r))
    log (Dual r d)   = Dual (log r) (d / r)

-- Floating doesn't include acoth, so I declared it separetly
-- arc-hyperbolic cotangent
acoth :: Floating a => a -> a
acoth x = (1/2) * log ((x + 1) / (x - 1))

-- arc-hyperbolic cotangent for Dual
acothD :: Floating a => Dual a -> Dual a 
acothD (Dual r d) = Dual (acoth r) (d / (1 - r ** 2))

derivative f x = dual (f (Dual x 1))

task4 = do
    let func1  = \x -> sin(2 * exp(x**2))
    let func1' = \x -> 4*x * exp(x**2) * cos(2 * exp (x**2)) 

    let func2  = \x -> x**3 - log(x**2) + 14*cos(x/2) + (acoth (x))**2
    let func2' = \x -> 3*x**2 - (2*acoth x )/(x**2 - 1) - 2/x - 7 * sin(x/2)

    putStrLn $ "Result for derivative sin(2 * e^(x^2)) in x0 = pi using:"
    putStrLn $ "Direct substitution into derivative: " ++ show (func1' pi)
    putStrLn $ "Automatic differentiation:           " ++ show (derivative func1 pi)
    putStrLn $ ""

    putStrLn $ "Result for derivative x^3 - log(x^2) + 14*cos(x/2) + (acoth(x))^2 in x0 = pi using:"
    putStrLn $ "Direct substitution into derivative: " ++ show (func2' pi)
    putStrLn $ "Automatic differentiation:           " ++ show (derivative func2 pi)

main = do
    putStrLn "--TASK 1--"
    task1
    putStrLn "\n--TASK 2--"
    task2
    putStrLn "\n--TASK 3--"
    task3
    putStrLn "--TASK 4--"
    task4
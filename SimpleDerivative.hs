import Data.Char
import Data.Maybe
import Data.List

prec :: String -> Int
prec "(" = 0
prec "+" = 1
prec "-" = 1
prec "*" = 2
prec "/" = 2
prec _ = error "!!!undefined operator!!!"

-- return true when o1 has lower or equal precedens to o2 
lowerOrEqualPrecedence :: String -> String -> Bool
lowerOrEqualPrecedence o1 o2 = prec o1 <= prec o2


tokenize :: String -> [String]
tokenize [] = []
tokenize (x:rest) | elem x "+-*/()" = ([x]:tokenize rest)
                  | isAlpha x = ([x]:tokenize rest)
                  | isDigit x = let
                    (number,r) = span (\c -> isDigit c || c == '.') (x:rest)
                    in (number :tokenize r)
                  | x == ' ' = tokenize rest
                  | otherwise = error "Wrong Input"


-- infix to postfix notation
toPostfix :: String -> [String]
toPostfix correct_infix = shuntingYard (tokenize correct_infix) []


shuntingYard :: [String] -> [String] -> [String]
shuntingYard [] stack = stack
shuntingYard (token:rest) stack | isAlphaNum (head token) = token : shuntingYard rest stack
                                | elem (head token) "+-*/" = let
                                    (stack_prefix,new_stack) = span (lowerOrEqualPrecedence token) stack
                                    in stack_prefix ++ shuntingYard rest (token:new_stack)
                                | (head token) == '(' = shuntingYard rest (token:stack) 
                                | (head token) == ')' = let
                                    (stack_prefix,new_stack) = span ((/=) "(") stack
                                    in if (new_stack /= []) 
                                    then stack_prefix ++ shuntingYard rest (tail new_stack) 
                                    else error "!!!Fatal error, Parentheses doesn't match!!!"
                                | otherwise = error "Syntax error"

-- AST data type
data Tree = Op Tree String Tree | Var String | Const Float
    deriving(Show)

-- build AST from the postfix and return root
buildAST :: [String] -> [Tree] -> Tree
buildAST [] [] = error "!!!error during construction of the AST!!!"
buildAST [] [root] = root 
buildAST [] (_:_:_) = error "!!!error during construction of the AST!!!"
buildAST (token:rest) stack | isDigit (head token) = buildAST rest (Const (read token :: Float) :stack)
                            | isAlpha (head token) = buildAST rest (Var token:stack)
                            | otherwise = let
                                (right:left:rest_of_stack) = stack
                                in buildAST rest (Op left token right:rest_of_stack)

infixToAST :: String -> Tree
infixToAST f = buildAST (toPostfix f) []


printAST :: Tree -> String
printAST (Const c) = show c 
printAST (Var x) = x
printAST (Op left o right) = "(" ++ (printAST left) ++ " " ++ o ++ " " ++ (printAST right) ++ ")"


treeDerivative :: Tree -> Tree
treeDerivative (Var _) = Const 1.0
treeDerivative (Const _) = Const 0.0
treeDerivative (Op left o right) = let
    leftDerivative = treeDerivative left
    rightDerivative = treeDerivative right
    l = simplifyTree left
    r = simplifyTree right
    in case o of 
        "+" -> simplifyTerm
         leftDerivative "+" rightDerivative
        "-" -> simplifyTerm
         leftDerivative "-" rightDerivative
        "*" -> simplifyTerm
         (simplifyTerm
         leftDerivative "*" r) "+" (simplifyTerm
         l "*" rightDerivative)
        "/" -> simplifyTerm
         (simplifyTerm
         (simplifyTerm
         leftDerivative "*" r) "-" (simplifyTerm
         l "*" rightDerivative)) "/" (simplifyTerm
          r "*" r )
        _ -> error "Unknown operator"

computeTree :: Tree -> [(String,Float)] -> Float
computeTree (Const c) _ = c
computeTree (Var x) dict = fromJust (lookup x dict)
computeTree (Op left o right) dict | o == "+" = l + r
                                   | o == "*" = l * r
                                   | o == "-" = l - r
                                   | o == "/" = l / r
                                   | otherwise = error "Unknown operator"
                                   where 
                                    l = computeTree left dict
                                    r = computeTree right dict 


simplifyTree :: Tree -> Tree
simplifyTree (Var x) = Var x
simplifyTree (Const c) = Const c
simplifyTree (Op left o right) = simplifyTerm
 (simplifyTree left) o (simplifyTree right)


simplifyTerm
 :: Tree -> String -> Tree -> Tree
simplifyTerm
 (Const a) o (Const b) | o == "+" = Const (a + b)
                               | o == "-" = Const (a - b)
                               | o == "*" = Const (a * b)
                               | o == "/" = if isInt a && isInt b then  simplifyFraction a b 
                                            else Const (a / b)
simplifyTerm
 (Const 0.0) "+" right = right
simplifyTerm
 (Const 0.0) "*"   _   = Const 0.0
simplifyTerm
 (Const 1.0) "*" right = right
simplifyTerm
 (Const 0.0) "/"   _   = Const 0.0
simplifyTerm
 left "+" (Const 0.0) = left
simplifyTerm
 left "-" (Const 0.0) = left
simplifyTerm
   _  "*" (Const 0.0) = Const 0.0
simplifyTerm
 left "*" (Const 1.0) = left
simplifyTerm
   _  "/" (Const 0.0) = error "Zero division" 
simplifyTerm
 left "/" (Const 1.0) = left
simplifyTerm
 left o right = Op left o right

                             
simplifyFraction :: Float -> Float -> Tree 
simplifyFraction numerator denominator =
    if denom == 1 then Const num else Op (Const num) "/" (Const denom)
    where
        d = customGcd numerator denominator
        (num, denom) = (numerator / d, denominator / d)

customGcd :: Float -> Float -> Float
customGcd a b = fromInteger (gcd (floor a) (floor b))

isInt :: Float -> Bool
isInt n = fromInteger (floor n) == n


-- if the infix has only one variable return itself otherwise throw an exception
onlyOneVariable :: String -> String
onlyOneVariable f = let 
    v = [c |c <- f, isAlpha c]
    in if length (nub v) <= 1 then f
       else error "!!!More than one variable!!!"


-- do derivative of a function with a single variable in {a..z,A..Z}, input has to be in infix notation
derivative :: String -> String
derivative f = printAST $ treeDerivative $ infixToAST $ onlyOneVariable f

compute :: String -> [(String,Float)] -> Float
compute f dict = computeTree (infixToAST f) dict

simplify:: String -> String
simplify f =  printAST $ simplifyTree $ infixToAST f

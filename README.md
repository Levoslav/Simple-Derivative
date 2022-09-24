---
Semester project 
Name: SimpleDerivative
Author: Jakub Hajko
class: Non-procedural programming [ NPRG005 ]
Year: 2022/23 (Second year summer semester)
---

README
    SimpleDerivative is a tool that is able to find a derivative of a simple function. Program is also capable of simplifying functions (SIMPLIFICATION RULES) and calculating the value of the function at an element x(substitute Float number).


INTERFACE FUNCTIONS
    derivative :: String -> String
    derivative "x / 0.5 + 4 * x" ----> "6.0"

    compute :: String -> [ (String,Float) ] -> Float
    compute "X / 6 * ( Y + X )" [("X",2.0),("Y",1.0)] ----> "1.0"

    simplify :: String -> String
    simplify "( x + 0 * 123 * x * 1.234 * x * z * 55 ) / 2" ---> "(x / 2)"

    Note: Input has to be in the infix notation and each token IN {constant,variable,+,-,*,/,(,)}.


SIMPLIFICATIONA RULES
    0.0*X = X*0.0 = 0.0 
    1.0*X = X*1.0 = X
    A * B = F    where A,B,F are Float constants
    X/0.0 = Error Zero divison
    0.0/X  = 0.0
    X/1.0 = X
    A/B = C/D    where A,B,C,D are positive Int constants and C/D is A/B simplified to the base form
    A/B = F    where A,B,F are Float constants
    0.0+X = X+0.0 = X 
    A + B = F    where A,B,F are Float constants
    X-0.0 = X


DERIVATIVE RULES
    c' = 0 (constant)
    x' = 1
    (a + b)' = a' + b'
    (a - b)' = a' - b'
    (a * b)' = a' * b + a * b'
    (a / b)' = (a' * b - a * b') / (b * b)


TEST DATA
    "2 + 3 * x - 1"
    "( 2 + 3 ) * x - 1"
    "x / 0.5 + 4 * x"
    "( 8 + 45.5 * 2 - 10 ) * x * x"
    "x * ( 15 + 32.25 / 0.25 * 0.5 )"
    "1 + 1.5 / ( x * 8 )"
    "4 * x * x * x - 3 * x * x + 2 * x - 1"
    
    correct results:
    3
    5
    6
    ((89 * x) + (89 * x))
    79.5
    (-12 / ((x * 8) * (x * 8)))
    ((((((4 * x) + (4 * x)) * x) + ((4 * x) * x)) - ((3 * x) + (3 * x))) + 2)

    ( I borrowed the data from my recodex c++ assignment )

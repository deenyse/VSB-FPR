import Distribution.Simple.Utils (xargs)
data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char
    deriving (Eq)

eval :: Expr -> Int
eval (Num x) = x
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b

showExpr :: Expr -> String
showExpr (Num x) = show x
showExpr (Add a b) = "(" ++ showExpr a ++ "+" ++ showExpr b ++ ")"
showExpr (Sub a b) = "(" ++ showExpr a ++ "-" ++ showExpr b ++ ")"
showExpr (Mul a b) = "(" ++  showExpr a ++ "*" ++ showExpr b ++ ")"
showExpr (Div a b) = "(" ++ showExpr a ++ "/" ++ showExpr b ++ ")"
showExpr (Var x) = [x]

instance (Show Expr) where
    show = showExpr


deriv :: Expr -> Char -> Expr
deriv (Num _) dx = (Num 0)
deriv (Var x) dx = if 
deriv (Add a b) 
deriv (Sub a b)
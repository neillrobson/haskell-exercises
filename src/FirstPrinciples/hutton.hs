data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b

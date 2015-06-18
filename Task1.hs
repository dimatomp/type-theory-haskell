import LambdaExpression

showWithParen :: LambdaExpr -> ShowS
showWithParen (Var name) = showString name
showWithParen (Lam par rec) = showParen True $ showChar '\\' . showString par . showChar '.' . showWithParen rec
showWithParen (App f s) = showParen True $ showWithParen f . showChar ' ' . showWithParen s

main = readFile "task1.in" >>= writeFile "task1.out" . (`showWithParen` "") . parseLambda

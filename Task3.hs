import Control.Lens
import Control.Monad
import Control.Monad.Trans.State

import Data.Maybe

import LambdaExpression
import Parser

replacement :: Parser (LambdaExpr, String, LambdaExpr)
replacement = liftM3 (,,) lambdaExpr (spacedChar '[' >> spaced lexeme) (spacedChar ':' >> char '=' >> lambdaExpr)

main = do
    (lambda, from, to) <- liftM (fromJust . evalStateT replacement) $ readFile "task3.in"
    writeFile "task3.out" $ (++ "\n") $ case lReduce from to lambda of
        Right answer -> show answer
        Left var -> "Нет свободы для подстановки переменной " ++ var

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State

import Data.Maybe

import LambdaExpression

replacement :: Parser (LambdaExpr, String, LambdaExpr)
replacement = liftM3 (,,) lambdaExpr (spacedChar ']' >> spaced lexeme) (spaced $ char ':' >> char '=' >> lambdaExpr)

main = do
    (lambda, from, to) <- liftM (fst . fromJust . runStateT replacement) $ readFile "task3.in"
    writeFile "task3.out" $ case lReduce from to lambda of
        Just answer -> show answer
        Nothing -> "Нет свободы для подстановки переменной " ++ from

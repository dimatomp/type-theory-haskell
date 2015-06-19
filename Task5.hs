import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State

import Data.Maybe
import Data.List

import Parser

import System.IO

import Unification

separates :: Char -> Parser a -> Parser [a]
separates c p = liftM2 (:) p (many $ char c >> p)

term :: Parser Term
term = lexeme >>= \name -> liftM (Fun name) params <|> return (Pri name)
    where params = char '(' >> ',' `separates` term <* char ')'

equation :: Parser Equation
equation = liftM2 (,) term (char '=' >> term)

system :: Parser [Equation]
system = '\n' `separates` equation

main = do
    eqs <- liftM (fromJust . evalStateT system) $ readFile "task5.in"
    fOut <- openFile "task5.out" WriteMode
    case unify eqs of
      Nothing -> hPutStrLn fOut "Система неразрешима"
      Just list -> forM_ list $ \(n, v) -> hPutStrLn fOut $ n ++ '=':show v
    hClose fOut

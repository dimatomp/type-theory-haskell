{-# LANGUAGE TemplateHaskell #-}

module LambdaExpression where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Hashable
import Data.Maybe

data LambdaExpr = Var { _name :: String }
                | Lam { _param :: String, _subst :: LambdaExpr }
                | App { _first :: LambdaExpr, _second :: LambdaExpr }
                deriving Eq
makeLenses ''LambdaExpr

instance Show LambdaExpr where
    showsPrec _ (Var name) = showString name
    showsPrec n (Lam par expr) = showParen (n > 0) $ showChar '\\' . showString par . showChar '.' . showsPrec 0 expr
    showsPrec n (App f s) = showParen (n > 1) $ showsPrec 1 f . showChar ' ' . showsPrec 2 s

instance Hashable LambdaExpr where
    hashWithSalt p = hashWithSalt p . show

type Parser = StateT String Maybe

allow :: (Char -> Bool) -> Parser Char
allow f = StateT $ \str -> uncons str >>= liftM2 (<$) id (_1 $ guard . f)

char :: Char -> Parser ()
char c = () <$ allow (== c)

lexeme :: Parser String
lexeme = some $ allow $ liftA2 (||) isAlphaNum (== '\'')

spaced = (many (allow isSpace) >>)

lambdaExpr :: Parser LambdaExpr
lambdaExpr = liftM (foldl1 App) $ some $ spaced unit
    where
        unit = braces <|> (Var <$> lexeme) <|> parseLam
        parseLam = liftM2 Lam (spaced (char '\\') >> spaced lexeme) (spaced (char '.') >> spaced lambdaExpr)
        braces = spaced (char '(') >> spaced lambdaExpr >>= (<$ spaced (char ')'))

parseLambda = fst . fromJust . runStateT lambdaExpr

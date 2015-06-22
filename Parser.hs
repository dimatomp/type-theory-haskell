module Parser where

import Control.Applicative
import Control.Monad.State
import Control.Lens

import Data.Char

type Parser = StateT String Maybe

allow :: (Char -> Bool) -> Parser Char
allow f = StateT $ \str -> uncons str >>= liftM2 (<$) id (_1 $ guard . f)

char :: Char -> Parser ()
char c = () <$ allow (== c)

lexeme :: Parser String
lexeme = some $ allow $ liftA2 (||) isAlphaNum (== '\'')

spaced = (many (allow isSpace) >>)
spacedChar = spaced . char

